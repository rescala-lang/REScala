package kofre.encrdt.lattices
import kofre.Lattice
import scala.math.Ordering.Implicits.infixOrderingOps

/** Lattice with the least-upper-bound defined by the timeStamp.
  * Timestamps must be unique, totally ordered, consistent with causal order.
  */
case class LastWriterWins[A, T](payload: A, timestamp: T)

object LastWriterWins {
  implicit def LWWRegLattice[T, O](using Ordering[O]): Lattice[LastWriterWins[T, O]] =
    (left, right) => if left.timestamp <= right.timestamp then right else left
}
