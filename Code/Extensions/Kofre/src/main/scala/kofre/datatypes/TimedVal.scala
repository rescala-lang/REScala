package kofre.datatypes

import kofre.base.{DecomposeLattice, Id}

import scala.math.Ordering.Implicits.infixOrderingOps

/** TimedVal is a case class for values that allows chronological ordering of values based on their time of creation.
  * In the case that two values from two different replicas have the exact same timestamp, the lexical ordering of the
  * ids of the two replicas is used to decide the ordering of the values. If two values from the same replica have the
  * same timestamp, then the higher-resolution local nanoTime is used to decide ordering.
  *
  * Instead of the default constructor, it is recommended that you use the apply method of the companion object which
  * automatically fills in the timestamp and nanoTime using System.currentTimeMillis() and System.nanoTime() respectively.
  */
case class TimedVal[A](value: A, timestamp: Long, replicaID: Id, nanoTime: Long)

object TimedVal {

  private val timedValOrdering: Ordering[TimedVal[Any]] =
    val tuporder = Ordering.Tuple3[Long, String, Long]
    (left, right) =>
      tuporder.compare(
        (left.timestamp, Id unwrap left.replicaID, left.nanoTime),
        (right.timestamp, Id unwrap right.replicaID, right.nanoTime),
      )

  given ordering[A]: Ordering[TimedVal[A]] = timedValOrdering.asInstanceOf

  def apply[A](value: A, replicaID: Id): TimedVal[A] =
    TimedVal(value, System.currentTimeMillis(), replicaID, System.nanoTime())

  implicit def decomposeLattice[A]: DecomposeLattice[TimedVal[A]] = new DecomposeLattice[TimedVal[A]] {
    override def lteq(left: TimedVal[A], right: TimedVal[A]): Boolean = ordering.lteq(left, right)

    /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: TimedVal[A]): Iterable[TimedVal[A]] = List(state)

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: TimedVal[A], right: TimedVal[A]): TimedVal[A] =
      if lteq(left, right) then left else right
  }
}
