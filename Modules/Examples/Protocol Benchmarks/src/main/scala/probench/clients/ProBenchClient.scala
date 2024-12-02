package probench.clients

import probench.data.*
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.syntax.DeltaBuffer

import java.util.concurrent.Semaphore

class ProBenchClient(val name: Uid) extends Client(name) {
  given localUid: LocalUid = LocalUid(name)
  private val dataManager  = ProDataManager[ClientNodeState](localUid, Bottom[ClientNodeState].empty, onStateChange)

  private var currentOp: Option[Request] = None
  val requestSemaphore                   = new Semaphore(0)

  private def onStateChange(oldState: ClientNodeState, newState: ClientNodeState): Unit = {
    for {
      op                                                     <- currentOp
      res @ Response(req, _) <- newState.responses.data.elements if req == op
    } {
      println(res.payload)

      currentOp = None

      dataManager.transform(_.mod(state => state.copy(responses = state.responses.mod(_.remove(res).toObrem))))

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
      current.mod(it => it.copy(requests = it.requests.mod(_.add(req).toObrem)))
    }

    requestSemaphore.acquire(1)
  }

  export dataManager.addLatentConnection

}
