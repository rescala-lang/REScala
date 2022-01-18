package kofre.primitives

import kofre.IdUtil.Id
import kofre.{IdUtil, Lattice}

import scala.collection.immutable.HashMap
import scala.math.PartialOrdering

case class LamportClock(replicaId: Id, time: Long) {
  def advance: LamportClock = LamportClock(replicaId, time + 1)
}

case class VectorClock(timestamps: Map[Id, Long]) {

  def advance(replicaId: Id): VectorClock = VectorClock(
    timestamps = timestamps + (replicaId -> (timestamps.getOrElse(replicaId, 0L) + 1L))
  )

  def timeOf(replicaId: Id): Long = timestamps.getOrElse(replicaId, 0)

  def clockOf(replicaId: Id): LamportClock = LamportClock(replicaId, timeOf(replicaId))

  def inc(id: Id): VectorClock    = VectorClock(HashMap(id -> (timestamps.getOrElse(id, 0L) + 1)))
  def <=(o: VectorClock): Boolean = timestamps.forall((k, v) => v <= o.timestamps.getOrElse(k, 0L))
  def <(o: VectorClock): Boolean  = this <= o && timestamps.exists((k, v) => v < o.timestamps.getOrElse(k, 0L))
}

object VectorClock {

  def zero: VectorClock                      = VectorClock(HashMap.empty)
  def fromMap(m: Map[Id, Long]): VectorClock = VectorClock(m)

  given lattice: Lattice[VectorClock] =
    given Lattice[Long] = _ max _
    Lattice.derived

  implicit object VectorClockOrdering extends PartialOrdering[VectorClock] {
    override def tryCompare(x: VectorClock, y: VectorClock): Option[Int] = {
      if x < y then return Some(-1)
      if y < x then return Some(1)
      if x <= y && y <= x then return Some(0)
      None
    }

    override def lteq(x: VectorClock, y: VectorClock): Boolean = x <= y
  }
}
