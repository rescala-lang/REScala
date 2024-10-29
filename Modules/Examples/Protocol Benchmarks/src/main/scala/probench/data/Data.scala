package probench.data

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.contextual.CausalQueue
import rdts.datatypes.{GrowOnlyList, GrowOnlyMap}
import rdts.dotted.Dotted
import rdts.time.VectorClock

enum KVOperation[Key, Value] {
  def key: Key

  case Read(key: Key)
  case Write(key: Key, value: Value)
}

case class Request(op: KVOperation[String, String], requestUid: Uid = Uid.gen())
case class Response(request: Request, payload: String)

case class ClientNodeState(
    requests: Dotted[CausalQueue[Request]],
    responses: Dotted[CausalQueue[Response]],
) derives Lattice, Bottom

