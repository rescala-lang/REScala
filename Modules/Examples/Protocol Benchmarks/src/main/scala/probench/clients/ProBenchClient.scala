package probench.clients

import probench.data.*
import probench.data.RequestResponseQueue.*
import rdts.base.LocalUid.replicaId
import rdts.base.{LocalUid, Uid}
import replication.DeltaDissemination

import java.util.concurrent.Semaphore

class ProBenchClient(val name: Uid, blocking: Boolean = true) extends Client(name) {
  given localUid: LocalUid = LocalUid(name)
  private val dataManager  = DeltaDissemination[State](localUid, handleIncoming, immediateForward = true)

  inline def log(inline msg: String): Unit =
    if false then println(s"[$name] $msg")

  val requestSemaphore = new Semaphore(0)

  type State = KVState
  var currentState: State      = KVState.empty
  val currentStateLock: AnyRef = new {}

  def publish(delta: State): State = currentStateLock.synchronized {
    if delta `inflates` currentState then {
      log(s"publishing")
      currentState = currentState.merge(delta)
      dataManager.applyDelta(delta)
    } else
      log(s"skip")
    currentState
  }

  def transform(f: State => State): State = publish(
    f(currentStateLock.synchronized(currentState))
  )

  def handleIncoming(change: State): Unit = currentStateLock.synchronized {
    log(s"handling incoming")
    val (old, changed) = currentStateLock.synchronized {
      val old = currentState
      currentState = currentState `merge` change
      (old, currentState)
    }
    if old != changed then {
      assert(changed == currentState)
      maybeHandleResponses(changed)
      // else log(s"upkept: ${pprint(upkept)}")
    }
  }

  private def maybeHandleResponses(newState: State): Unit =
    val (requests, responses) = (newState.requests.requests, newState.requests.responses)
    for {
      req @ Req(_, _, timestamp) <- requests.get(replicaId).map(_.value).getOrElse(Set())
      if responses.contains(timestamp)
    } {
      responses.get(timestamp) match
        case Some(res) =>
          println(res.value)
          transform(state => KVState(requests = state.requests.complete(req)))
          if blocking then requestSemaphore.release(1)
        case None => ()
    }

  override def handleOpImpl(op: KVOperation[String, String]): Unit =
    // TODO: still not sure that the semaphore use is correct …
    // its quite likely possible that some other request is answered after draining, causing the code below to return immediately
    // though overall currentOp is not protected at all, so it is triple unclear what is going on
    if blocking then
      requestSemaphore.drainPermits()
      ()

    val _ = transform(state => KVState(requests = state.requests.request(op)))

    if blocking then requestSemaphore.acquire(1)

  export dataManager.addLatentConnection

}
