package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import probench.data.*
import probench.data.RequestResponseQueue.{Req, Res}
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.Membership
import rdts.datatypes.experiments.protocols.simplified.Paxos
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer
import rdts.time.Dot
import replication.DeltaDissemination

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Time {

  var current: Long = System.nanoTime()

  def report(name: => String = ""): Unit = if false then
    println {
      synchronized {
        val last = current
        current = System.nanoTime()
        s"$name took ${(current - last).doubleValue / 1000_000}ms"
      }
    }
}

class KeyValueReplica(val uid: Uid, val votingReplicas: Set[Uid]) {

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
    DeltaDissemination(localUid, handleIncoming)

  def handleIncoming(change: ClusterState): Unit = {
    val (old, changed) = currentStateLock.synchronized {
      val old = currentState
      currentState = currentState `merge` change
      (old, currentState)
    }
    val upkept = changed.upkeep()
    val merged = currentStateLock.synchronized {
      currentState = currentState.merge(upkept)
      currentState
    }
    clusterDataManager.applyDelta(upkept)
    maybeAnswerClient(old, merged)
  }

  private val kvCache = mutable.HashMap[String, String]()

  private def onClientStateChange(oldState: ClientNodeState, newState: ClientNodeState): Unit = {
    newState.firstUnansweredRequest.foreach { req =>
      clusterDataManager.applyDelta {
        currentStateLock.synchronized {
          val delta = currentState.write(ClusterData(req.value, req.dot))
          currentState = currentState.merge(delta)
          delta
        }
      }
    }
  }

  var counter = 0

  private def maybeAnswerClient(oldState: ClusterState, newState: ClusterState): Unit = {

    val start = System.nanoTime()
    var last  = start
    val tid = synchronized {
      counter += +1
      counter
    }

    Time.report(s"[$tid] cluster changed")

    def timeStep(msg: => String): Unit = if false then
      println {
        val current = last
        last = System.nanoTime()
        s"[$tid] $msg after ${(last - current).doubleValue / 1000_000}ms"
      }

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

    timeStep("done")
    if false then println(s"[$tid] total ${(System.nanoTime() - start).doubleValue / 1000_000}ms")
  }

  export clientDataManager.addLatentConnection as addClientConnection
  export clusterDataManager.addLatentConnection as addClusterConnection

}
