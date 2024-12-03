package probench.data

import rdts.base.{Bottom, Lattice, Uid}
import rdts.datatypes.contextual.{ReplicatedSet}
import rdts.dotted.{Dotted, Obrem}
import rdts.time.Dot

enum KVOperation[Key, Value] {
  def key: Key

  case Read(key: Key)
  case Write(key: Key, value: Value)
}

type ClientNodeState = Dotted[RequestResponseQueue[KVOperation[String, String], String]]

case class ClusterData(op: KVOperation[String, String], origin: Dot)
