package de.ckuessner
package encrdt.causality

import encrdt.util.MapHelper.max

import scala.math.PartialOrdering

case class VectorClock(timestamps: Map[String, Long] = Map()) {
  def merged(other: VectorClock): VectorClock = VectorClock(max(timestamps, other.timestamps))

  def advance(replicaId: String): VectorClock = VectorClock(
    timestamps = timestamps + (replicaId -> (timestamps.getOrElse(replicaId, 0L) + 1L))
  )

  def timeOf(replicaId: String): Long = timestamps.getOrElse(replicaId, 0)

  def clockOf(replicaId: String): LamportClock = LamportClock(timeOf(replicaId), replicaId)
}

object VectorClock {
  implicit object VectorClockOrdering extends PartialOrdering[VectorClock] {
    override def tryCompare(x: VectorClock, y: VectorClock): Option[Int] = {
      if (x.timestamps.isEmpty) return Some(0)
      if (x.timestamps.keySet != y.timestamps.keySet) return None

      val clockPairs = x.timestamps.keySet.map(key => (x.timestamps(key), y.timestamps(key)))
      val comparisons = clockPairs map { case (x, y) => x compare y }

      if (comparisons.max < 0) return Some(-1)
      if (comparisons.min > 0) return Some(1)
      if (comparisons.min == 0 && comparisons.max == 0) return Some(0)
      return None
    }

    override def lteq(x: VectorClock, y: VectorClock): Boolean = {
      if (x.timestamps.isEmpty && y.timestamps.isEmpty) return true
      if (x.timestamps.keySet != y.timestamps.keySet) return false

      val anyXGreaterY = x.timestamps.keySet
        .exists(key => x.timestamps(key) > y.timestamps(key))

      !anyXGreaterY
    }
  }
}
