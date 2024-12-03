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

object Time {

  var current: Long = System.nanoTime()

  def report(name: => String = ""): Unit = if true then
    println {
      synchronized {
        val last = current
        current = System.nanoTime()
        s"$name took ${(current - last).doubleValue / 1000_000}ms"
      }
    }
}

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
      { incoming =>
        executionContext.execute(() => handleIncoming(incoming))
      }
    )

  def publish(delta: ClusterState): ClusterState = currentStateLock.synchronized {
    if !(delta <= currentState) then {
      currentState = currentState.merge(delta)
      executionContext.execute(() => clusterDataManager.applyDelta(delta))
    }
    currentState
  }

  def transform(f: ClusterState => ClusterState) = publish(
    f(currentStateLock.synchronized(currentState))
  )

  def handleIncoming(change: ClusterState): Unit = {
    val (old, changed) = currentStateLock.synchronized {
      val old = currentState
      currentState = currentState `merge` change
      (old, currentState)
    }
    val upkept = changed.upkeep()
    maybeAnswerClient(old, publish(upkept))
  }

  private val kvCache = mutable.HashMap[String, String]()

  private def onClientStateChange(oldState: ClientNodeState, newState: ClientNodeState): Unit = {
    newState.firstUnansweredRequest.foreach { req =>
      println(s"applying client request $req on $uid")
      transform(_.write(ClusterData(req.value, req.dot)))
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
