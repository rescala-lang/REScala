package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import probench.data.*
import probench.data.RequestResponseQueue.Req
import rdts.base.Lattice.syntax
import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.{MultiPaxos, Participants}
import rdts.syntax.DeltaBuffer
import replication.DeltaDissemination

import scala.collection.mutable

class KeyValueReplica(val uid: Uid, val votingReplicas: Set[Uid]) {

  inline def log(inline msg: String): Unit =
    if false then println(s"[$uid] $msg")

  given Participants(votingReplicas)
  private type ClusterState = MultiPaxos[ClusterData]

  given localUid: LocalUid = LocalUid(uid)

  val clientDataManager: ProDataManager[ClientNodeState] =
    ProDataManager[ClientNodeState](
      localUid,
      Bottom[ClientNodeState].empty,
      onClientStateChange,
      immediateForward = true
    )

  val currentStateLock: AnyRef   = new {}
  var currentState: ClusterState = MultiPaxos()

  val clusterDataManager: DeltaDissemination[ClusterState] =
    DeltaDissemination(
      localUid,
      handleIncoming,
      immediateForward = true
    )

  // propose myself as leader if I have the lowest id
  votingReplicas.minOption match
    case Some(id) if id == uid => currentStateLock.synchronized { transform(_.startLeaderElection) }
    case _                     => ()

  def publish(delta: ClusterState): ClusterState = currentStateLock.synchronized {
    if delta `inflates` currentState then {
      log(s"publishing")
      currentState = currentState.merge(delta)
      clusterDataManager.applyDelta(delta)
    } else
      log(s"skip")
    currentState
  }

  def forceUpkeep(): ClusterState = currentStateLock.synchronized {
    publish(currentState.upkeep)
  }

  def needsUpkeep(): Boolean = currentStateLock.synchronized {
    val state = currentState
    val delta = state.upkeep
    state != (state `merge` delta)
  }

  def transform(f: ClusterState => ClusterState): ClusterState = publish(
    f(currentStateLock.synchronized(currentState))
  )

  def handleIncoming(change: ClusterState): Unit = currentStateLock.synchronized {
    log(s"handling incoming")
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
    }
  }

  private val kvCache = mutable.HashMap[String, String]()

  private def onClientStateChange(oldState: ClientNodeState, newState: ClientNodeState): Unit = {
    if currentState.leader.contains(replicaId) then // only propose if this replica is the current leader
      newState.firstUnansweredRequest.foreach { req =>
        log(s"applying client request $req")
        currentStateLock.synchronized { transform(_.proposeIfLeader(ClusterData(req.value, req.dot))) }
      }
    else
      log("Not the leader. Ignoring request for now.")
  }

  private def maybeAnswerClient(oldState: ClusterState, newState: ClusterState): Unit = {

    log(s"${newState.log}")
    // println(s"${pprint.tokenize(newState).mkString("")}")

    for decidedRequest <- newState.readDecisionsSince(oldState.rounds.counter) do {
      val decision: String = decidedRequest match {
        case ClusterData(KVOperation.Read(key), _) =>
          kvCache.synchronized {
            kvCache.getOrElse(key, s"Key '$key' has not been written to!")
          }
        case ClusterData(KVOperation.Write(key, value), _) =>
          kvCache.synchronized {
            kvCache.put(key, value)
          }
          s"$key=$value; OK"
      }

      clientDataManager.transform { it =>
        it.state.requests.collectFirst { case req if req.dot == decidedRequest.origin => req }.map { req =>
          it.mod(_.respond(req, decision))
        }.getOrElse(it)
      }

    }

  }

  export clientDataManager.addLatentConnection as addClientConnection
  export clusterDataManager.addLatentConnection as addClusterConnection

}
