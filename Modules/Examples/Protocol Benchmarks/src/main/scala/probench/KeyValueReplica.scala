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
import scala.concurrent.ExecutionContext

class KeyValueReplica(val uid: Uid, val votingReplicas: Set[Uid]) {

  val executionContext =
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
      println(s"[$uid] publishing")
      currentState = currentState.merge(delta)
      executionContext.execute(() => clusterDataManager.applyDelta(delta))
    } else
      println(s"[$uid] skip")
    currentState
  }

  def transform(f: ClusterState => ClusterState) = publish(
    f(currentStateLock.synchronized(currentState))
  )

  def handleIncoming(change: ClusterState): Unit = currentStateLock.synchronized {
    println(s"[$uid] handling incoming")
    val (old, changed) = currentStateLock.synchronized {
      val old = currentState
      currentState = currentState `merge` change
      (old, currentState)
    }
    val upkept = changed.upkeep()
    if upkept <= currentState
    then println(s"[$uid] no changes")
    else println(s"[$uid] upkeep")
    assert(changed == currentState)
    // else println(s"[$uid] upkept: ${pprint(upkept)}")
    val newState = publish(upkept)
    maybeAnswerClient(old, newState)
  }

  private val kvCache = mutable.HashMap[String, String]()

  private def onClientStateChange(oldState: ClientNodeState, newState: ClientNodeState): Unit = {
    newState.firstUnansweredRequest.foreach { req =>
      println(s"[$uid] applying client request $req")
      currentStateLock.synchronized { transform(_.write(ClusterData(req.value, req.dot))) }
    }
  }

  private def maybeAnswerClient(oldState: ClusterState, newState: ClusterState): Unit = {

    println(s"[$uid] ${newState.log}")
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

      /*
      val clientState = clientDataManager.mergedState

      if clientState.requests.data.elements.nonEmpty then {
        clusterDataManager.applyDelta {
          currentStateLock.synchronized {
            val delta = currentState.write(clientState.requests.data.elements.head)
            currentState = currentState.merge(delta)
            delta
          }
        }
      }
       */
    }

  }

  export clientDataManager.addLatentConnection as addClientConnection
  export clusterDataManager.addLatentConnection as addClusterConnection

}
