package probench.data

import probench.data.RequestResponseQueue.Timestamp
import rdts.time.Dot

enum KVOperation[Key, Value] {
  def key: Key

  case Read(key: Key)
  case Write(key: Key, value: Value)
}

type ClientNodeState = RequestResponseQueue[KVOperation[String, String], String]

case class ClusterData(op: KVOperation[String, String], origin: Timestamp)
