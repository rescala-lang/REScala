package probench.data

import probench.data.RequestResponseQueue.{Req, Res, Timestamp}
import rdts.base.*
import rdts.base.Lattice.mapLattice
import rdts.base.LocalUid.replicaId
import rdts.datatypes.{Epoch, GrowOnlySet}
import rdts.dotted.FilteredLattice
import rdts.time.CausalTime

import scala.collection.immutable

// responses should be a remove wins set/map
case class RemoveWinsMap[K, V](inner: Map[K, V] = Map.empty[K, V], tombstones: GrowOnlySet[K] = GrowOnlySet.empty[K]):
  def insert(key: K, value: V): RemoveWinsMap[K, V] =
    if tombstones.contains(key) then
      RemoveWinsMap()
    else
      RemoveWinsMap(Map(key -> value))

  def remove(key: K): RemoveWinsMap[K, V] = RemoveWinsMap(tombstones = Set(key))

  def value: Map[K, V] = inner

object RemoveWinsMap:
  def empty[K, V]: RemoveWinsMap[K, V] = RemoveWinsMap()
  given [K, V]: Lattice[Map[K, V]] =
    given Lattice[V] = Lattice.assertEquals
    Lattice.mapLattice
  given [K, V]: FilteredLattice[RemoveWinsMap[K, V]](Lattice.derived) with
    // remove answered requests from queue
    override def filter(
        base: RemoveWinsMap[K, V],
        other: RemoveWinsMap[K, V]
    ): RemoveWinsMap[K, V] =
      RemoveWinsMap(
        inner = base.inner.filter {
          case (key, _) => !other.tombstones.contains(key)
        },
      )

case class RequestResponseQueue[S, T](
    requests: Map[Uid, Epoch[Set[Req[S]]]] = Map.empty[Uid, Epoch[Set[Req[S]]]],
    responses: Map[Timestamp, Res[S, T]] = Map.empty[Timestamp, Res[S, T]]
) {
  def request(value: S)(using LocalUid): RequestResponseQueue[S, T] =
    // find the newest timestamp
    val timestamp = requestsSorted.map(_.timestamp).lastOption match
      case Some((time, _)) => (time.advance, replicaId)
      case None            => (CausalTime.now(), replicaId)

    val myRequests = requests.getOrElse(replicaId, Epoch.empty[Set[Req[S]]])
    val updated    = myRequests.value + Req(value, replicaId, timestamp)
    RequestResponseQueue(requests = Map(replicaId -> myRequests.write(updated)))

//  def respond(requestTimestamp: Timestamp, value: T)(using LocalUid): RequestResponseQueue[S, T] =
//    respond(requests(requestTimestamp), value)

  def respond(request: Req[S], value: T)(using LocalUid): RequestResponseQueue[S, T] =
    RequestResponseQueue(responses =
      Map(request.timestamp -> Res(value = value, request = request))
    )

  def responseTo(req: Req[S]): Option[Res[S, T]] =
    responses.get(req.timestamp)

  /** Completes a request by removing it from the set of requests.
    * This is supposed to be done by the replica where the request originated.
    */
  def complete(request: Req[S])(using LocalUid): RequestResponseQueue[S, T] =
    val currentRequests = requests.getOrElse(replicaId, Epoch.empty[Set[Req[S]]])
    RequestResponseQueue(
      requests = Map(replicaId -> currentRequests.epocheWrite(
        currentRequests.value - request
      ))
    )

//  def consumeResponse(res: Res[S, T]): RequestResponseQueue[S, T] =
//    RequestResponseQueue(responses = responses.remove(res.request.timestamp))
//
  def firstUnansweredRequest: Option[Req[S]] =
    requestsSorted.find(responseTo(_).isEmpty)

  def requestsSorted: List[Req[S]] =
    val currentRequests: Iterable[Req[S]] = requests.flatMap {
      case (nodeId, epoch) => epoch.value
    }
    currentRequests.toList.sortBy(_.timestamp)
}

object RequestResponseQueue {
  type Timestamp = (CausalTime, Uid)
  case class Req[+T](value: T, requester: Uid, timestamp: Timestamp)
  case class Res[+S, +T](value: T, request: Req[S])

  def empty[S, T]: RequestResponseQueue[S, T] = RequestResponseQueue()

  given bottomInstance[S, T]: Bottom[RequestResponseQueue[S, T]] = Bottom.provide(empty)

  given Ordering[Timestamp] = Orderings.lexicographic

  // lattices
//  given l1[A]: Lattice[Map[Timestamp, Req[A]]] = // for the requestMap
//    given Lattice[Req[A]] = Lattice.assertEquals
//    Lattice.mapLattice
  given l2[S, T]: Lattice[Map[Timestamp, Res[S, T]]] = // for the requestMap
    given Lattice[Res[S, T]] = Lattice.assertEquals
    Lattice.mapLattice

  given [S, T]: Lattice[RequestResponseQueue[S, T]] = Lattice.derived
//  given [S, T]: FilteredLattice[RequestResponseQueue[S, T]](Lattice.derived) with
//    // remove responses that lack matching request
//    override def filter(
//        base: RequestResponseQueue[S, T],
//        other: RequestResponseQueue[S, T]
//    ): RequestResponseQueue[S, T] =
//      val newlyAnswered: Set[Timestamp] = other.responses.value.keys.toSet
//      RequestResponseQueue(
//        requests = base.requests.filter {
//          case (timestamp, _) => !newlyAnswered.contains(timestamp)
//        },
//        responses = base.responses
//      )
}
