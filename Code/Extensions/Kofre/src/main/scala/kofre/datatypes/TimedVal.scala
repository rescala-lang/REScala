package kofre.datatypes

import kofre.base.DecomposeLattice
import kofre.base.Defs.Id
import math.Ordering.Implicits.infixOrderingOps

/** TimedVal is a case class for values that allows chronological ordering of values based on their time of creation.
  * In the case that two values from two different replicas have the exact same timestamp, the lexical ordering of the
  * ids of the two replicas is used to decide the ordering of the values. If two values from the same replica have the
  * same timestamp, then the higher-resolution local nanoTime is used to decide ordering.
  *
  * Instead of the default constructor, it is recommended that you use the apply method of the companion object which
  * automatically fills in the timestamp and nanoTime using System.currentTimeMillis() and System.nanoTime() respectively.
  */
case class TimedVal[A](value: A, replicaID: Id, nanoTime: Long, timestamp: Long) {
  def laterThan(other: TimedVal[A]): Boolean =
    this.timestamp > other.timestamp ||
    this.timestamp == other.timestamp &&
    (
      this.replicaID > other.replicaID ||
      this.replicaID == other.replicaID && this.nanoTime > other.nanoTime
    )
}

object TimedVal {
  def apply[A](value: A, replicaID: Id): TimedVal[A] =
    TimedVal(value, replicaID, System.nanoTime(), System.currentTimeMillis())

  implicit def decomposeLattice[A]: DecomposeLattice[TimedVal[A]] = new DecomposeLattice[TimedVal[A]] {
    override def lteq(left: TimedVal[A], right: TimedVal[A]): Boolean = !left.laterThan(right)

    /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: TimedVal[A]): Iterable[TimedVal[A]] = List(state)

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: TimedVal[A], right: TimedVal[A]): TimedVal[A] =
      if (left.laterThan(right)) left else right
  }
}
