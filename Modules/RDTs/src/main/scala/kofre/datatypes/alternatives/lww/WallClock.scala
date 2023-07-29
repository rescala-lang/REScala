package kofre.datatypes.alternatives.lww

import kofre.base.{Lattice, Uid}


/** WallClock is a case class for values that allows chronological ordering of values based on their time of creation.
  * In the case that two values from two different replicas have the exact same timestamp, the lexical ordering of the
  * ids of the two replicas is used to decide the ordering of the values. If two values from the same replica have the
  * same timestamp, then the higher-resolution local nanoTime is used to decide ordering.
  *
  * Instead of the default constructor, it is recommended that you use the [[WallClock.now]] method of the companion object which
  * automatically fills in the timestamp and nanoTime using System.currentTimeMillis() and System.nanoTime() respectively.
  */
case class WallClock(timestamp: Long, replicaID: Uid, nanoTime: Long)

object WallClock {

  def now(replicaId: Uid): WallClock = WallClock(System.currentTimeMillis(), replicaId, System.nanoTime())

  given ordering: Ordering[WallClock] =
    Ordering.by[WallClock, Long](_.timestamp)
      .orElseBy(wc => Uid.unwrap(wc.replicaID))
      .orElseBy(_.nanoTime)

  given lattice: Lattice[WallClock] = Lattice.fromOrdering(ordering)

}
