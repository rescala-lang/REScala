package probench.data

import probench.data.RequestResponseQueue.{Req, Res, Timestamp}
import rdts.base.*
import rdts.base.Lattice.mapLattice
import rdts.base.LocalUid.replicaId
import rdts.datatypes.Epoch
import rdts.time.CausalTime

case class RequestResponseQueue[S, T](
    requests: Map[Uid, Epoch[Set[Req[S]]]] = Map.empty[Uid, Epoch[Set[Req[S]]]],
    responses: Map[Timestamp, Res[T]] = Map.empty[Timestamp, Res[T]]
) {
  def request(value: S)(using LocalUid): RequestResponseQueue[S, T] =
    // find the newest timestamp
    val timestamp = requestsSorted.lastOption.map(_.timestamp) match
      case Some((time, _)) => (time.advance, replicaId)
      case None            => (CausalTime.now(), replicaId)

    val myRequests = requests.getOrElse(replicaId, Epoch.empty[Set[Req[S]]])
    val updated    = myRequests.value + Req(value, replicaId, timestamp)
    RequestResponseQueue(requests = Map(replicaId -> myRequests.write(updated)))

  def respond(request: Req[S], value: T)(using LocalUid): RequestResponseQueue[S, T] =
    RequestResponseQueue(responses =
      Map(request.timestamp -> Res(value))
    )

  def responseTo(req: Req[S]): Option[Res[T]] =
    responses.get(req.timestamp)

  /** Completes a request by removing it from the set of requests.
    * This is supposed to be done by the replica where the request originated.
    */
  def complete(request: Req[S])(using LocalUid): RequestResponseQueue[S, T] =
    val myRequests = requests.getOrElse(replicaId, Epoch.empty[Set[Req[S]]])
    RequestResponseQueue(
      requests = Map(replicaId -> myRequests.epocheWrite(
        myRequests.value - request
      ))
    )

  def firstUnansweredRequest: Option[Req[S]] =
    requestsSorted.find(responseTo(_).isEmpty)

  def requestsSorted: List[Req[S]] =
    val allRequests: Iterable[Req[S]] = requests.flatMap {
      case (nodeId, epoch) => epoch.value
    }
    allRequests.toList.sortBy(_.timestamp)
}

object RequestResponseQueue {
  type Timestamp = (CausalTime, Uid)

  case class Req[+T](value: T, requester: Uid, timestamp: Timestamp)
  case class Res[+T](value: T)

  def empty[S, T]: RequestResponseQueue[S, T] = RequestResponseQueue()

  given bottomInstance[S, T]: Bottom[RequestResponseQueue[S, T]] = Bottom.provide(empty)

  given Ordering[Timestamp] = Orderings.lexicographic

  // lattices
  given [S, T]: Lattice[Map[Timestamp, Res[T]]] = // for the requestMap
    given Lattice[Res[T]] = Lattice.assertEquals
    Lattice.mapLattice

  given [S, T]: Lattice[RequestResponseQueue[S, T]] = Lattice.derived
}
