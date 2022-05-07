package kofre.encrdt.lattices
import kofre.Lattice

import java.time.Instant

/** Lattice with the least-upper-bound defined by the timeStamp.
  * Timestamps must be unique, totally ordered, consistent with causal order.
  */
case class LastWriterWins[T, A](timestamp: T, payload: A)

object LastWriterWins {
  implicit def LWWRegLattice[O, T](using Ordering[O]): Lattice[LastWriterWins[O, T]] =
    (left, right) => if Ordering[O].lteq(left.timestamp, right.timestamp) then right else left

  given [O: Ordering, A]: Ordering[LastWriterWins[O, A]] with {
    override def compare(x: LastWriterWins[O, A], y: LastWriterWins[O, A]): Int = Ordering[O].compare(x.timestamp, y.timestamp)
  }
}
