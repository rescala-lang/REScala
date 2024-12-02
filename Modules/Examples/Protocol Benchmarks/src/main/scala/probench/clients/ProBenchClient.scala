package probench.clients

import probench.data.*
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.syntax.DeltaBuffer
import probench.data.RequestResponseQueue.*

import java.util.concurrent.Semaphore

class ProBenchClient(val name: Uid) extends Client(name) {
  given localUid: LocalUid = LocalUid(name)
  private val dataManager  = ProDataManager[ClientNodeState](localUid, Bottom[ClientNodeState].empty, onStateChange)

  val requestSemaphore = new Semaphore(0)

  private def onStateChange(oldState: ClientNodeState, newState: ClientNodeState): Unit = {
    for {
      Res(req @ Req(_, requester, _, _), value, _, _, _) <- newState.responses if requester == localUid
    } {
      println(value)

      dataManager.transform(_.mod(_.complete(req)))

      requestSemaphore.release(1)
    }
  }

  override def handleOpImpl(op: KVOperation[String, String]): Unit = {
    // TODO: still not sure that the semaphore use is correct …
    // its quite likely possible that some other request is answered after draining, causing the code below to return immediately
    // though overall currentOp is not protected at all, so it is triple unclear what is going on
    requestSemaphore.drainPermits()

    dataManager.transform { current =>
      current.mod(_.request(op))
    }

    requestSemaphore.acquire(1)
  }

  export dataManager.addLatentConnection

}
