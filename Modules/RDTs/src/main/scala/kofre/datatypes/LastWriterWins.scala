package kofre.datatypes

import kofre.base.{Lattice, Uid}
import kofre.time.WallClock

import java.time.Instant
import scala.math.Ordering.Implicits.infixOrderingOps

case class TimedVal[A](timestamp: WallClock, payload: A) extends GenericLastWriterWins[WallClock, A]

/** Lattice with the least-upper-bound defined by the timeStamp.
  * Timestamps must be unique, totally ordered, consistent with causal order.
  */
case class LastWriterWins[Time, Value](timestamp: Time, payload: Value) extends GenericLastWriterWins[Time, Value]


object TimedVal:
  def now[Value](payload: Value, replicaId: Uid): TimedVal[Value] =
    TimedVal(WallClock.now(replicaId), payload)

trait GenericLastWriterWins[Time, Value] {
  def timestamp: Time
  def payload: Value
}

object GenericLastWriterWins {

  given lattice[Time: Ordering, A, LWW <: GenericLastWriterWins[Time, A]]: Lattice[LWW] =
    new Lattice[LWW] {
      override def lteq(left: LWW, right: LWW): Boolean =
        left.timestamp <= right.timestamp

      /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
      override def decompose(state: LWW): Iterable[LWW] = List(state)

      /** By assumption: associative, commutative, idempotent. */
      override def merge(left: LWW, right: LWW): LWW =
        Ordering[Time].compare(left.timestamp, right.timestamp) match
          case 0 => if (left.payload == right.payload) then left
            else throw IllegalStateException(s"LWW same timestamp, different value: »$left«, »$right«")
          case -1 => right
          case 1  => left
    }
}
