package kofre.causality

import kofre.IdUtil.Id
import kofre.causality.{Dot, VectorClock}
import kofre.{IdUtil, Lattice}

import scala.math.PartialOrdering

/** Dots are another name for lamport clocks.
  * Dots are globally unique counters that are used to track causality in causal CRDTs. To guarantee global uniqueness,
  * dots combine a globally unique replicaID with a locally unique counter.
  */
case class Dot(replicaId: Id, time: Long) {
  def advance: Dot  = Dot(replicaId, time + 1)
  def next: Dot     = advance
  def replicaID: Id = replicaId
}

case class VectorClock(timestamps: Map[Id, Long]) {
  def timeOf(replicaId: Id): Long = timestamps.getOrElse(replicaId, 0)

  def clockOf(replicaId: Id): Dot = Dot(replicaId, timeOf(replicaId))

  def inc(id: Id): VectorClock    = VectorClock(Map(id -> (timestamps.getOrElse(id, 0L) + 1)))
  def <=(o: VectorClock): Boolean = timestamps.forall((k, v) => v <= o.timestamps.getOrElse(k, 0L))
  def <(o: VectorClock): Boolean  = this <= o && timestamps.exists((k, v) => v < o.timestamps.getOrElse(k, 0L))
  def tryCompare(y: VectorClock): Option[Int] = {
    if this < y then return Some(-1)
    if y < this then return Some(1)
    if this <= y && y <= this then return Some(0)
    None
  }
}

object VectorClock {

  def zero: VectorClock                      = VectorClock(Map.empty)
  def fromMap(m: Map[Id, Long]): VectorClock = VectorClock(m)

  given lattice: Lattice[VectorClock] =
    given Lattice[Long] = _ max _
    Lattice.derived

  implicit object VectorClockOrdering extends PartialOrdering[VectorClock] {
    override def tryCompare(x: VectorClock, y: VectorClock): Option[Int] = x.tryCompare(y)

    override def lteq(x: VectorClock, y: VectorClock): Boolean = x <= y
  }
}
