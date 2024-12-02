package probench.data

import rdts.base.{Bottom, Lattice, Uid}
import rdts.datatypes.contextual.{CausalQueue, ReplicatedSet}
import rdts.dotted.{Dotted, Obrem}
import rdts.time.Dot

enum KVOperation[Key, Value] {
  def key: Key

  case Read(key: Key)
  case Write(key: Key, value: Value)
}

case class Request(op: KVOperation[String, String], requestUid: Uid = Uid.gen())
case class Response(request: Request, payload: String)

case class ClientNodeState(
    requests: Obrem[ReplicatedSet[Request]],
    responses: Obrem[ReplicatedSet[Response]],
) derives Lattice, Bottom
