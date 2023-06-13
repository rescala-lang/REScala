package kofre.time

import kofre.base.{Lattice, Time, Uid}

import scala.annotation.tailrec
import scala.math.PartialOrdering

case class VectorClock(timestamps: Map[Uid, Time]) {
  def timeOf(replicaId: Uid): Time = timestamps.getOrElse(replicaId, 0)

  def dotOf(replicaId: Uid): Dot = Dot(replicaId, timeOf(replicaId))

  def inc(id: Uid): VectorClock   = VectorClock(Map(id -> (timestamps.getOrElse(id, 0L) + 1)))
  def <=(o: VectorClock): Boolean = timestamps.forall((k, v) => v <= o.timestamps.getOrElse(k, 0L))
  def <(o: VectorClock): Boolean  = this <= o && timestamps.exists((k, v) => v < o.timestamps.getOrElse(k, 0L))
}

object VectorClock {

  def zero: VectorClock                       = VectorClock(Map.empty)
  def fromMap(m: Map[Uid, Time]): VectorClock = VectorClock(m)

  given lattice: Lattice[VectorClock] =
    given Lattice[Time] = _ max _
    Lattice.derived

  val vectorClockTotalOrdering: Ordering[VectorClock] = new Ordering[VectorClock] {
    override def compare(x: VectorClock, y: VectorClock): Int =
      vectorClockOrdering.tryCompare(x, y) match
        case Some(v) => v
        case None =>
          @tailrec
          def smaller(remaining: List[Uid]): Int = remaining match {
            case h :: t =>
              val l   = x.timestamps.getOrElse(h, 0L)
              val r   = y.timestamps.getOrElse(h, 0L)
              val res = Ordering[Time].compare(l, r)
              if (res == 0) then smaller(t) else res
            case Nil => 0
          }
          val ids = (x.timestamps.keysIterator ++ y.timestamps.keysIterator).toList.sorted
          smaller(ids)
  }

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
