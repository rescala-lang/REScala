package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import probench.data.*
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.Membership
import rdts.datatypes.experiments.protocols.simplified.Paxos
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer

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

class KeyValueReplika(val name: Uid, val initialClusterIds: Set[Uid]) {

  private type ClusterState = Membership[Request, Paxos, Paxos]

  given localUid: LocalUid = LocalUid(name)

  val clientDataManager: ProDataManager[ClientNodeState] =
    ProDataManager[ClientNodeState](
      localUid,
      Bottom[ClientNodeState].empty,
      onClientStateChange,
      immediateForward = true
    )

  val clusterDataManager: ProDataManager[ClusterState] =
    ProDataManager[ClusterState](localUid, Membership.init(initialClusterIds), onClusterStateChange)

  private val kvCache = mutable.HashMap[String, String]()

  private def onClientStateChange(oldState: ClientNodeState, newState: ClientNodeState): Unit = {

    if newState.requests.data.values.size == 1 then {
      Time.report("got request")
      clusterDataManager.transform(_.mod(_.write(newState.requests.data.head)))
      Time.report("transform done")
    }
  }

  var counter = 0

  private def onClusterStateChange(oldState: ClusterState, newState: ClusterState): Unit = {

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

    val delta                = newState.upkeep()
    val upkept: ClusterState = newState.merge(delta)
    timeStep("upkeep + merge")

    if !(upkept <= newState) || upkept.counter > newState.counter then {
      clusterDataManager.transform(_.mod(_ => delta))
      timeStep("some state changes maybe logs???")
    }

    for op <- upkept.readDecisionsSince(oldState.counter) do {
      val res: String = op match {
        case Request(KVOperation.Read(key), _) =>
          kvCache.synchronized {
            kvCache.getOrElse(key, s"Key '$key' has not been written to!")
          }
        case Request(KVOperation.Write(key, value), _) =>
          kvCache.synchronized {
            kvCache.put(key, value)
          }
          s"$key=$value; OK"
      }

      clientDataManager.transform { it =>
        if it.state.requests.data.values.exists { e => e.value == op } then {
          it.mod { state =>
            // println(s"Writing Response: $op -> $res")
            val newState = state.copy(
              requests = state.requests.mod(_.removeBy(_ == op)),
              responses = state.responses.mod(_.enqueue(Response(op, res))),
            )
            // println(s"Remaining Requests: ${newState.requests.data.values.toList.map(_.value)}")
            newState
          }.tap(_ =>
            timeStep("answering request")
          )
        } else it
      }

      val clientState = clientDataManager.mergedState

      if clientState.requests.data.values.nonEmpty then {
        clusterDataManager.transform(_.mod(_.write(clientState.requests.data.head)))
      }
    }

    timeStep("done")
    if false then println(s"[$tid] total ${(System.nanoTime() - start).doubleValue / 1000_000}ms")
  }

  export clientDataManager.addLatentConnection as addClientConnection
  export clusterDataManager.addLatentConnection as addClusterConnection

}
