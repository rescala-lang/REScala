package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import probench.data.*
import probench.data.RequestResponseQueue.Req
import rdts.base.Lattice.syntax
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.Membership
import rdts.datatypes.experiments.protocols.simplified.Paxos
import rdts.syntax.DeltaBuffer
import replication.DeltaDissemination

import java.util.concurrent.Executors
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class KeyValueReplica(val uid: Uid, val votingReplicas: Set[Uid]) {

  def log(msg: String): Unit =
    if false then println(s"[$uid] $msg")

  val executionContext: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor)

  private type ClusterState = Membership[ClusterData, Paxos, Paxos]

  given localUid: LocalUid = LocalUid(uid)

  val clientDataManager: ProDataManager[ClientNodeState] =
    ProDataManager[ClientNodeState](
      localUid,
      Bottom[ClientNodeState].empty,
      onClientStateChange,
      immediateForward = true
    )

  val currentStateLock: AnyRef   = new {}
  var currentState: ClusterState = Membership.init(votingReplicas)

  val clusterDataManager: DeltaDissemination[ClusterState] =
    DeltaDissemination(
      localUid,
      handleIncoming,
      immediateForward = true
    )

  def publish(delta: ClusterState): ClusterState = currentStateLock.synchronized {
    if !(delta <= currentState) then {
      log(s"publishing")
      currentState = currentState.merge(delta)
      executionContext.execute(() => clusterDataManager.applyDelta(delta))
    } else
      log(s"skip")
    currentState
  }

  def forceUpkeep(): ClusterState = currentStateLock.synchronized {
    publish(currentState.upkeep())
  }

  def needsUpkeep(): Boolean = currentStateLock.synchronized {
    val state = currentState
    val delta = state.upkeep()
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
      val upkept = changed.upkeep()
      if upkept <= currentState
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
    newState.firstUnansweredRequest.foreach { req =>
      log(s"applying client request $req")
      currentStateLock.synchronized { transform(_.write(ClusterData(req.value, req.dot))) }
    }
  }

  private def maybeAnswerClient(oldState: ClusterState, newState: ClusterState): Unit = {

    log(s"${newState.log}")
    // println(s"${pprint.tokenize(newState).mkString("")}")

    for decidedRequest <- newState.readDecisionsSince(oldState.counter) do {
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
