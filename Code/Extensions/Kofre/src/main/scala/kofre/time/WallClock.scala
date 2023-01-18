package kofre.time

import kofre.base.{Lattice, Id}

/** WallClock is a case class for values that allows chronological ordering of values based on their time of creation.
  * In the case that two values from two different replicas have the exact same timestamp, the lexical ordering of the
  * ids of the two replicas is used to decide the ordering of the values. If two values from the same replica have the
  * same timestamp, then the higher-resolution local nanoTime is used to decide ordering.
  *
  * Instead of the default constructor, it is recommended that you use the [[WallClock.now]] method of the companion object which
  * automatically fills in the timestamp and nanoTime using System.currentTimeMillis() and System.nanoTime() respectively.
  */
case class WallClock(timestamp: Long, replicaID: Id, nanoTime: Long)

object WallClock {

  def now(replicaId: Id): WallClock = WallClock(System.currentTimeMillis(), replicaId, System.nanoTime())

  given ordering[A]: Ordering[WallClock] =
    val tuporder = Ordering.Tuple3[Long, String, Long]
    (left, right) =>
      tuporder.compare(
        (left.timestamp, Id unwrap left.replicaID, left.nanoTime),
        (right.timestamp, Id unwrap right.replicaID, right.nanoTime),
      )

}
