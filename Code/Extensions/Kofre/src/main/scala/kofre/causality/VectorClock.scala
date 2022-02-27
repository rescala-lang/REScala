package kofre.causality

import kofre.Defs.Id
import kofre.causality.{Dot, VectorClock}
import kofre.{Defs, Lattice}

import scala.math.PartialOrdering

case class VectorClock(timestamps: Map[Id, Defs.Time]) {
  def timeOf(replicaId: Id): Defs.Time = timestamps.getOrElse(replicaId, 0)

  def clockOf(replicaId: Id): Dot = Dot(replicaId, timeOf(replicaId))

  def inc(id: Id): VectorClock    = VectorClock(Map(id -> (timestamps.getOrElse(id, 0L) + 1)))
  def <=(o: VectorClock): Boolean = timestamps.forall((k, v) => v <= o.timestamps.getOrElse(k, 0L))
  def <(o: VectorClock): Boolean  = this <= o && timestamps.exists((k, v) => v < o.timestamps.getOrElse(k, 0L))
}

object VectorClock {

  def zero: VectorClock                           = VectorClock(Map.empty)
  def fromMap(m: Map[Id, Defs.Time]): VectorClock = VectorClock(m)

  given lattice: Lattice[VectorClock] =
    given Lattice[Defs.Time] = _ max _
    Lattice.derived

  given vectorClockOrdering: PartialOrdering[VectorClock] = new PartialOrdering[VectorClock] {
    override def tryCompare(x: VectorClock, y: VectorClock): Option[Int] = {
      if x < y then return Some(-1)
      if y < x then return Some(1)
      if x <= y && y <= x then return Some(0)
      None
    }

    override def lteq(x: VectorClock, y: VectorClock): Boolean = x <= y
  }
}
