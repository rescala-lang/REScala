package probench.data

import probench.data.RequestResponseQueue.{Req, Timestamp}
import rdts.time.Dot

enum KVOperation[Key, Value] {
  def key: Key

  case Read(key: Key)
  case Write(key: Key, value: Value)
}

type ClientNodeState = RequestResponseQueue[KVOperation[String, String], String]

case class ClusterData(request: Req[KVOperation[String, String]]):
  def op: KVOperation[String, String] = request.value
