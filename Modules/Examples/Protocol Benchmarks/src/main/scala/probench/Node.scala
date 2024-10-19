package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import probench.data.*
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.simplified.Paxos
import rdts.datatypes.experiments.protocols.{LogHack, Membership}
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer

class Node(val name: String, val initialClusterIds: Set[Uid]) {

  private type ClusterState = Membership[Request, Paxos, Paxos]

  given localUid: LocalUid = LocalUid.predefined(name)
  private val clientDataManager =
    DataManager[ClientNodeState](localUid, Bottom[ClientNodeState].empty, onClientStateChange)
  private val clusterDataManager =
    DataManager[ClusterState](localUid, Membership.init(initialClusterIds), onClusterStateChange)

  private def onClientStateChange(oldState: ClientNodeState, newState: ClientNodeState): Unit = {
    if newState.requests.data.values.nonEmpty then {
      clusterDataManager.transform(_.mod(_.write(newState.requests.data.values.last.value)))
    }
  }

  given LogHack = new LogHack(true)

  private def onClusterStateChange(oldState: ClusterState, newState: ClusterState): Unit = {
    val upkept: ClusterState = newState.merge(newState.upkeep())

    if !(upkept <= newState) then {
      clusterDataManager.transform(_.mod(_ => upkept))
    }

    if newState.log.size > oldState.log.size then {
      val op = newState.log.last

      val res: String = op match {
        case Request(KVOperation.Read(key), _) =>
          newState.log.reverseIterator.collectFirst {
            case Request(KVOperation.Write(writeKey, value), _) if writeKey == key => value
          }.getOrElse(s"Key $key has not been written to!")
        case Request(KVOperation.Write(_, _), _) => "OK"
      }

      clientDataManager.transform(_.mod(state =>
        state.copy(
          requests = state.requests.mod(_.removeBy(_ == op)),
          responses = state.responses.mod(_.enqueue(Response(op, res))),
        )
      ))

    }
  }

  export clientDataManager.addLatentConnection as addClientConnection
  export clusterDataManager.addLatentConnection as addClusterConnection

}
