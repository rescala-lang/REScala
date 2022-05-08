package kofre.primitives

import kofre.base.Lattice

/** Lattice with the least-upper-bound defined by the timeStamp.
  * Timestamps must be unique, totally ordered, consistent with causal order.
  */
case class LastWriterWins[T, A](timestamp: T, payload: A)

object LastWriterWins {
  implicit def LWWRegLattice[O, T](using Ordering[O]): Lattice[LastWriterWins[O, T]] =
    (left, right) =>
      Ordering[O].compare(left.timestamp, right.timestamp) match
        case 0 => if (left == right) then left
          else throw IllegalStateException(s"LWW same timestamp, different value: »$left«, »$right«")
        case -1 => right
        case 1  => left

  given [O: Ordering, A]: Ordering[LastWriterWins[O, A]] with {
    override def compare(x: LastWriterWins[O, A], y: LastWriterWins[O, A]): Int =
      Ordering[O].compare(x.timestamp, y.timestamp)
  }
}
