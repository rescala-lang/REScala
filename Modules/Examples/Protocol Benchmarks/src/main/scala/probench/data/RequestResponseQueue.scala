package probench.data

import probench.data.RequestResponseQueue.{Req, Res, Timestamp}
import rdts.base.*
import rdts.base.Lattice.mapLattice
import rdts.base.LocalUid.replicaId
import rdts.datatypes.alternatives.ObserveRemoveSet
import rdts.dotted.FilteredLattice
import rdts.time.{CausalTime, Dot, VectorClock}

case class RequestResponseQueue[S, T](
    requests: Map[Timestamp, Req[S]] = Map.empty[Timestamp, Req[S]],
    responses: ObserveRemoveSet[Res[S, T]] = ObserveRemoveSet.empty[Res[S, T]]
) {
  def request(value: S)(using LocalUid): RequestResponseQueue[S, T] =
    // find the newest timestamp
    val timestamp = requests.keys.maxOption match
      case Some((time, uid)) => (time.advance, replicaId)
      case None              => (CausalTime.now(), replicaId)
    RequestResponseQueue(requests = Map(timestamp -> Req(value, replicaId)))

  def respond(requestTimestamp: Timestamp, value: T)(using LocalUid): RequestResponseQueue[S, T] =
    respond(requests(requestTimestamp), value)

  def respond(request: Req[S], value: T)(using LocalUid): RequestResponseQueue[S, T] =
    RequestResponseQueue(responses = ObserveRemoveSet(Set(Res(value = value, request = request))))

  def responseTo(req: Req[S]): Option[Res[S, T]] =
    responses.value.collectFirst {
      case res @ Res(_, r) if r == req => res
    }

  def consumeResponse(res: Res[S, T]): RequestResponseQueue[S, T] =
    RequestResponseQueue(responses = responses.remove(res))

  def firstUnansweredRequest: Option[(Timestamp, Req[S])] =
    requests.collectFirst { case r @ (timestamp, req) if responseTo(req).isEmpty => r }
}

object RequestResponseQueue {
  type Timestamp = (CausalTime, Uid)
  case class Req[+T](value: T, requester: Uid)
  case class Res[+S, +T](value: T, request: Req[S])

  def empty[S, T]: RequestResponseQueue[S, T] = RequestResponseQueue()

  given bottomInstance[S, T]: Bottom[RequestResponseQueue[S, T]] = Bottom.provide(empty)

  given Ordering[Timestamp] = Orderings.lexicographic

  // lattices
  given [A]: Lattice[Map[Timestamp, Req[A]]] = // for the requestMap
    given Lattice[Req[A]] = Lattice.assertEquals
    Lattice.mapLattice
  given [S, T]: FilteredLattice[RequestResponseQueue[S, T]](Lattice.derived) with
    // remove answered requests from queue
    override def filter(
        base: RequestResponseQueue[S, T],
        other: RequestResponseQueue[S, T]
    ): RequestResponseQueue[S, T] =
      val newlyAnswered: Set[Req[S]] = other.responses.value.map(_.request)
      RequestResponseQueue(
        requests = base.requests.filter {
          case (timestamp, req) => newlyAnswered.contains(req)
        },
        responses = base.responses
      )
}
