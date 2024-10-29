package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import probench.data.*
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.simplified.Paxos
import rdts.datatypes.experiments.protocols.{LogHack, Membership}
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer

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
      clusterDataManager.transform(_.mod(_.write(newState.requests.data.head)))
    }
  }

  private def onClusterStateChange(oldState: ClusterState, newState: ClusterState): Unit = {
    val delta                = newState.upkeep()
    val upkept: ClusterState = newState.merge(delta)

    if !(upkept <= newState) || upkept.log.size > newState.log.size then {
      clusterDataManager.transform(_.mod(_ => delta))
    }

    if upkept.log.size > oldState.log.size then {
      val diff = upkept.log.size - oldState.log.size
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
              //println(s"Remaining Requests: ${newState.requests.data.values.toList.map(_.value)}")
              newState
            }
          } else it
        }

        val clientState = clientDataManager.mergedState.data

        if clientState.requests.data.values.nonEmpty then {
          clusterDataManager.transform(_.mod(_.write(clientState.requests.data.head)))
        }
      }
    }
  }

  export clientDataManager.addLatentConnection as addClientConnection
  export clusterDataManager.addLatentConnection as addClusterConnection

}
