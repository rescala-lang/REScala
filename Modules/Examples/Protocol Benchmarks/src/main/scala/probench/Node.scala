package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import probench.data.*
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.simplified.Paxos
import rdts.datatypes.experiments.protocols.{LogHack, Membership}
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer

import scala.util.chaining.scalaUtilChainingOps

object Time {
  var current = System.nanoTime()
  def report(name: String = "") = synchronized {
    val last = current
    current = System.nanoTime()
    println(s"$name took ${(current - last).doubleValue / 1000_000}ms")
  }
}

class Node(val name: Uid, val initialClusterIds: Set[Uid]) {

  private type ClusterState = Membership[Request, Paxos, Paxos]

  given localUid: LocalUid = LocalUid(name)
  given LogHack            = new LogHack(false)

  private val clientDataManager =
    DataManager[ClientNodeState](localUid, Bottom[ClientNodeState].empty, onClientStateChange)
  private val clusterDataManager =
    DataManager[ClusterState](localUid, Membership.init(initialClusterIds), onClusterStateChange)

  private def onClientStateChange(oldState: ClientNodeState, newState: ClientNodeState): Unit = {
    /*
      val diff = newState.requests.data.values.size - oldState.requests.data.values.size
      if diff > 0 then {
        println(s"Requests: ${newState.requests.data.values.toList.map(_.value)}")
        println(s"Sorted  : ${newState.requests.data.values.toList.sortBy(_.order)(using VectorClock.vectorClockTotalOrdering).map(it => it.order -> it.value)}")
        println(s"Dots    : ${newState.requests.data.dots}")
        println(s"Time    : ${newState.requests.data.clock}")
      }
     */

    if newState.requests.data.values.size == 1 then {
      Time.report("got request")
      clusterDataManager.transform(_.mod(_.write(newState.requests.data.head)))
      Time.report("transform done")
    }
  }

  var counter = 0

  private def onClusterStateChange(oldState: ClusterState, newState: ClusterState): Unit = {

    val start = System.nanoTime()
    var last = start
    val tid =
      synchronized {
        counter = counter + 1
        counter
      }

    Time.report(s"[${tid}] cluster changed")

    def timeStep(msg: String) =
      val current = last
      last = System.nanoTime()
      println(s"[$tid] $msg after ${(last - current).doubleValue / 1000_000}ms")

    val delta                = newState.upkeep()
    val upkept: ClusterState = newState.merge(delta)
    val end                  = System.nanoTime()
    timeStep("upkeep + merge")

    if !(upkept <= newState) || upkept.log.size > newState.log.size then {
      clusterDataManager.transform(_.mod(_ => delta))
      timeStep("some state changes maybe logs???")
    }

    if upkept.log.size > oldState.log.size then {
      val diff: Int = upkept.log.size - oldState.log.size
      // println(s"DIFF $diff")

      for op <- upkept.log.reverseIterator.take(diff).toList.reverseIterator do {

        val res: String = op match {
          case Request(KVOperation.Read(key), _) =>
            upkept.log.reverseIterator.collectFirst {
              case Request(KVOperation.Write(writeKey, value), _) if writeKey == key => s"$key=$value"
            }.getOrElse(s"Key '$key' has not been written to!")
          case Request(KVOperation.Write(key, value), _) => s"$key=$value; OK"
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

        val clientState = clientDataManager.mergedState.data

        if clientState.requests.data.values.nonEmpty then {
          clusterDataManager.transform(_.mod(_.write(clientState.requests.data.head)))
        }
      }
    }

    timeStep("done")
    println(s"[${tid}] total ${(System.nanoTime() - start).doubleValue / 1000_000}ms")
  }

  export clientDataManager.addLatentConnection as addClientConnection
  export clusterDataManager.addLatentConnection as addClusterConnection

}
