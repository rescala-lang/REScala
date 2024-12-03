package probench.data

import probench.data.RequestResponseQueue.{Req, Res}
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.dotted.{Dotted, HasDots}
import rdts.time.{Dot, Dots, VectorClock}

import scala.collection.immutable.Queue

case class RequestResponseQueue[S, T](
    requests: Queue[Req[S]],
    responses: Queue[Res[S, T]],
    processed: Map[LocalUid, VectorClock],
    clock: VectorClock
) {

  type Delta = RequestResponseQueue[S, T]

  def request(value: S)(using id: LocalUid): Delta = {
    val time = clock.merge(clock.inc(LocalUid.replicaId))
    val dot  = time.dotOf(LocalUid.replicaId)

    RequestResponseQueue(
      Queue(Req(value, id, dot, time)),
      Queue.empty,
      Map.empty,
      time
    )
  }

  def respond(request: Req[S], value: T)(using id: LocalUid): Delta = {
    val time = clock.merge(clock.inc(LocalUid.replicaId))
    val dot  = time.dotOf(LocalUid.replicaId)

    RequestResponseQueue(
      Queue.empty,
      Queue(Res(request, value, id, dot, time)),
      Map.empty,
      time
    )
  }

  def responsesTo(req: Req[S]): Seq[Res[S, T]] =
    responses.filter(res => res.request == req)

  def firstUnansweredRequest: Option[Req[S]] =
    requests.collectFirst { case r: Req[S] if responsesTo(r).isEmpty => r }

  def complete(req: Req[S])(using id: LocalUid): Delta = {
    val time = clock.merge(clock.inc(LocalUid.replicaId))

    RequestResponseQueue(
      Queue.empty,
      Queue.empty,
      Map(id -> req.order),
      time
    )
  }

}

object RequestResponseQueue {
  case class Req[+T](value: T, requester: LocalUid, dot: Dot, order: VectorClock)
  case class Res[+S, +T](request: Req[S], value: T, responder: LocalUid, dot: Dot, order: VectorClock)

  def empty[S, T]: RequestResponseQueue[S, T] =
    RequestResponseQueue(Queue.empty, Queue.empty, Map.empty, VectorClock.zero)

  given hasDots[S, T]: HasDots[RequestResponseQueue[S, T]] with {
    extension (value: RequestResponseQueue[S, T])
      override def dots: Dots = Dots.from(value.requests.view.map(_.dot) ++ value.responses.view.map(_.dot))

      override def removeDots(dots: Dots): Option[RequestResponseQueue[S, T]] =
        Some(
          RequestResponseQueue(
            value.requests.filter(req => !dots.contains(req.dot)),
            value.responses.filter(res => !dots.contains(res.dot)),
            value.processed,
            value.clock
          )
        )
  }

  given procLattice: Lattice[Map[LocalUid, VectorClock]]         = Lattice.mapLattice
  given bottomInstance[S, T]: Bottom[RequestResponseQueue[S, T]] = Bottom.derived

  given lattice[S, T]: Lattice[RequestResponseQueue[S, T]] with {
    override def merge(
        left: RequestResponseQueue[S, T],
        right: RequestResponseQueue[S, T]
    ): RequestResponseQueue[S, T] =
      val processed = left.processed.merge(right.processed)

      RequestResponseQueue(
        (left.requests concat right.requests)
          .filter { req => processed.get(req.requester).forall(time => !(req.order <= time)) }
          .sortBy { req => req.order }(using Ordering.fromLessThan(VectorClock.vectorClockOrdering.lt))
          .distinct,
        (left.responses concat right.responses)
          .filter { req => processed.get(req.request.requester).forall(time => !(req.request.order <= time)) }
          .sortBy { req => req.order }(using Ordering.fromLessThan(VectorClock.vectorClockOrdering.lt))
          .distinct,
        processed,
        left.clock.merge(right.clock)
      )
  }

}
