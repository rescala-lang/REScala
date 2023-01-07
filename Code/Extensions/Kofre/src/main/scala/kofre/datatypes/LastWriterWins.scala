package kofre.datatypes

import kofre.base.Lattice

import java.time.Instant

/** Lattice with the least-upper-bound defined by the timeStamp.
  * Timestamps must be unique, totally ordered, consistent with causal order.
  */
case class LastWriterWins[Time, Value](timestamp: Time, payload: Value)

object LastWriterWins {

  def now[Value](payload: Value): LastWriterWins[Instant, Value] = LastWriterWins(Instant.now(), payload)

  given lattice[Time, Value](using Ordering[Time]): Lattice[LastWriterWins[Time, Value]] =
    (left, right) =>
      Ordering[Time].compare(left.timestamp, right.timestamp) match
        case 0 => if (left.payload == right.payload) then left
          else throw IllegalStateException(s"LWW same timestamp, different value: »$left«, »$right«")
        case -1 => right
        case 1  => left

  given [O: Ordering, A]: Ordering[LastWriterWins[O, A]] with {
    override def compare(x: LastWriterWins[O, A], y: LastWriterWins[O, A]): Int =
      Ordering[O].compare(x.timestamp, y.timestamp)
  }
}
