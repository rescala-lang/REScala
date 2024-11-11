package probench.clients

import probench.benchmark.{BenchmarkData, CSVWriter}
import probench.data.*
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.contextual.CausalQueue
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer

import java.nio.file.Path
import java.util.concurrent.Semaphore
import scala.collection.mutable
import scala.io.StdIn
import scala.io.StdIn.readLine
import scala.util.matching.Regex

class ProBenchClient(val name: Uid) extends Client(name) {
  given localUid: LocalUid = LocalUid(name)
  private val dataManager  = ProDataManager[ClientNodeState](localUid, Bottom[ClientNodeState].empty, onStateChange)

  private var currentOp: Option[Request] = None
  val requestSemaphore                   = new Semaphore(0)

  private def onStateChange(oldState: ClientNodeState, newState: ClientNodeState): Unit = {
    for {
      op                                                     <- currentOp
      CausalQueue.QueueElement(res @ Response(req, _), _, _) <- newState.responses.data.values if req == op
    } {
      println(res.payload)

      currentOp = None

      dataManager.transform(_.mod(state => state.copy(responses = state.responses.mod(_.removeBy(_ == res)))))

      requestSemaphore.release(1)
    }
  }

  override def handleOpImpl(op: KVOperation[String, String]): Unit = {
    val req = Request(op)
    currentOp = Some(req)
    
    // TODO: still not sure that the semaphore use is correct …
    // its quite likely possible that some other request is answered after draining, causing the code below to return immediately
    // though overall currentOp is not protected at all, so it is triple unclear what is going on
    requestSemaphore.drainPermits()
    
    dataManager.transform { current =>
      current.mod(it => it.copy(requests = it.requests.mod(_.enqueue(req))))
    }

    requestSemaphore.acquire(1)
  }
  
  export dataManager.addLatentConnection

}
