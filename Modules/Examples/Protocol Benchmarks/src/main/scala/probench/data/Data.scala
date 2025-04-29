package probench.data

import probench.data.RequestResponseQueue.Req
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.experiments.protocols.{MultiPaxos, Participants}

enum KVOperation[Key, Value] {
  def key: Key

  case Read(key: Key)
  case Write(key: Key, value: Value)
}

type ConnInformation = Map[LocalUid, LastWriterWins[Long]]
type ClusterState    = MultiPaxos[Req[KVOperation[String, String]]]
type ClientState     = RequestResponseQueue[KVOperation[String, String], String]

case class KVState(
    requests: RequestResponseQueue[KVOperation[String, String], String] = RequestResponseQueue.empty,
    clusterState: MultiPaxos[Req[KVOperation[String, String]]] = MultiPaxos.empty
):
  def upkeep(using LocalUid, Participants): KVState =
    KVState(clusterState = clusterState.upkeep)

object KVState:
  given Lattice[KVState] =
    Lattice.derived

  def empty: KVState = KVState()
