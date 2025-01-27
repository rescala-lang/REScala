package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import probench.data.*
import probench.data.RequestResponseQueue.Req
import rdts.base.Lattice.syntax
import rdts.base.LocalUid.replicaId
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.experiments.protocols.{MultiPaxos, MultipaxosPhase, Participants}
import replication.DeltaDissemination

import scala.collection.mutable

class KeyValueReplica(val uid: Uid, val votingReplicas: Set[Uid]) {

  inline def log(inline msg: String): Unit =
    if false then println(s"[$uid] $msg")

  given Participants(votingReplicas)

  given localUid: LocalUid = LocalUid(uid)

  val currentStateLock: AnyRef = new {}
  private type State = KVState
  var currentState: State = KVState.empty

  val clusterDataManager: DeltaDissemination[State] =
    DeltaDissemination(
      localUid,
      handleIncoming,
      immediateForward = true
    )

  // propose myself as leader if I have the lowest id
  votingReplicas.minOption match
    case Some(id) if id == uid =>
      transform(current => KVState(clusterState = current.clusterState.startLeaderElection))
    case _ => ()

  def publish(delta: State): State = currentStateLock.synchronized {
    if delta `inflates` currentState then {
      log(s"publishing")
      currentState = currentState.merge(delta)
      clusterDataManager.applyDelta(delta)
    } else
      log(s"skip")
    currentState
  }

  def forceUpkeep(): State = currentStateLock.synchronized {
    publish(currentState.upkeep)
  }

  def needsUpkeep(): Boolean = currentStateLock.synchronized {
    val state = currentState
    val delta = state.upkeep
    state != (state `merge` delta)
  }

  def transform(f: State => State): State = publish(
    f(currentStateLock.synchronized(currentState))
  )

  def handleIncoming(change: State): Unit = currentStateLock.synchronized {
    log(s"handling incoming $change")
    val (old, changed) = currentStateLock.synchronized {
      val old = currentState
      currentState = currentState `merge` change
      (old, currentState)
    }
    if old != changed then {
      val upkept = changed.upkeep
      if currentState.subsumes(upkept)
      then log(s"no changes")
      else log(s"upkeep")
      assert(changed == currentState)
      // else log(s"upkept: ${pprint(upkept)}")
      val newState = publish(upkept)
      maybeAnswerClient(old, newState)
      maybeProposeNewValue(old, newState)
    }
  }

  private val kvCache = mutable.HashMap[String, String]()

  private def maybeProposeNewValue(oldState: State, newState: State)(using LocalUid): Unit = {
    // check if we are the leader and ready to handle a request
    if newState.clusterState.leader.contains(replicaId) && newState.clusterState.phase == MultipaxosPhase.Idle then
      // ready to propose value
      newState.requests.firstUnansweredRequest match
        case Some(req) =>
          log(s"Proposing new value $req.")
          currentStateLock.synchronized {
            val after = transform(state => KVState(clusterState = state.clusterState.proposeIfLeader(req)))
          }
//          val after = publish(KVState(clusterState = newState.clusterState.proposeIfLeader(req)))
//          log(s"after proposal: $after")
        case None =>
          log("I am the leader but request queue is empty.")
  }

  private def maybeAnswerClient(oldState: State, newState: State): Unit = {

    log(s"${newState.clusterState.log}")
    // println(s"${pprint.tokenize(newState).mkString("")}")

    for req @ Req(op, _, _) <- newState.clusterState.readDecisionsSince(oldState.clusterState.rounds.counter) do {
      val decision: String = op match {
        case KVOperation.Read(key) =>
          kvCache.synchronized {
            kvCache.getOrElse(key, s"Key '$key' has not been written to!")
          }
        case KVOperation.Write(key, value) =>
          kvCache.synchronized {
            kvCache.put(key, value)
          }
          s"$key=$value; OK"
      }

      transform { state => KVState(requests = state.requests.respond(req, decision)) }

    }

  }

  export clusterDataManager.addLatentConnection as addClientConnection
  export clusterDataManager.addLatentConnection as addClusterConnection

}
