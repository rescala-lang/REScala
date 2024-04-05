package com.github.ckuessner.encrdt.lattices

/** Lattice with the least-upper-bound defined by the timeStamp. Timestamps must be unique, totally ordered, consistent
  * with causal order.
  *
  * @param value
  *   the value of the register
  * @param timestamp
  *   the associated value, that provides the order to the value
  * @tparam T
  *   type of value
  * @tparam O
  *   type of timestamp
  */
case class LastWriterWinsRegisterLattice[T, O](value: T, timestamp: O)

object LastWriterWinsRegisterLattice {
  given LWWRegLattice[T, O <: Ordered[O]]: SemiLattice[LastWriterWinsRegisterLattice[T, O]] = (left, right) => {
    if (left == right) left
    else if (left.timestamp > right.timestamp) left
    else if (left.timestamp < right.timestamp) right
    else throw new IllegalArgumentException(s"$left and $right can't be ordered")
  }

  given LWWRegLatticeWithOrdering[T, O](using ord: Ordering[O]): SemiLattice[LastWriterWinsRegisterLattice[T, O]] =
    (left, right) => {
      val order = ord.compare(left.timestamp, right.timestamp)
      if (left == right) left
      else if (order > 0) left
      else if (order < 0) right
      else throw new IllegalArgumentException(s"$left and $right can't be ordered")
    }
}
