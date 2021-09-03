package de.ckuessner
package encrdt.lattices

import encrdt.causality.VectorClock
import encrdt.lattices.LWWTime.lwwTimeOrd
import encrdt.lattices.interfaces.SemiLattice

import java.time.Instant
import scala.math.PartialOrdering

class LastWriterWinsRegister[T](initialState: LastWriterWinsRegisterLattice[T, LWWTime],
                                val replicaId: String) { // SemiLattice requires ordering of timestamp

  private var _state = initialState

  def state: LastWriterWinsRegisterLattice[T, LWWTime] = _state

  def value: T = state.value

  def set(value: T): Unit = {
    _state = LastWriterWinsRegisterLattice(value, _state.timestamp.advance(replicaId))
  }

  def merge(otherState: LastWriterWinsRegisterLattice[T, LWWTime]): Unit =
    _state = SemiLattice.merged(this.state, otherState)
}

object LastWriterWinsRegister {
  def apply[T](replicaId: String, initialValue: T): LastWriterWinsRegister[T] =
    new LastWriterWinsRegister(
      LastWriterWinsRegisterLattice(
        initialValue,
        LWWTime().advance(replicaId)),
      replicaId)
}

/**
 * Lattice with the least-upper-bound defined by the timeStamp.
 * Timestamps must be unique, totally ordered, consistent with causal order.
 *
 * @param value     the value of the register
 * @param timestamp the associated value, that provides the order to the value
 * @tparam T type of value
 * @tparam O type of timestamp
 */
case class LastWriterWinsRegisterLattice[T, O](value: T, timestamp: O)

case class LWWTime(vectorClock: VectorClock = VectorClock(),
                   utc: Instant = Instant.ofEpochMilli(0),
                   replicaId: String = ""
                  ) extends Ordered[LWWTime] {

  def advance(rId: String): LWWTime = LWWTime(vectorClock.advance(rId), Instant.now(), rId)

  override def compare(that: LWWTime): Int = lwwTimeOrd.compare(this, that)
}

object LWWTime {
  implicit def lwwTimeOrd: Ordering[LWWTime] =
    (l, r) => Ordering[(VectorClock, Instant, String)].compare(unapply(l).get, unapply(r).get)

  implicit def causallyConsistentTimeReplicaOrd: Ordering[(VectorClock, Instant, String)] = (l, r) => {
    if (PartialOrdering[VectorClock].gt(l._1, r._1)) 1
    else if (PartialOrdering[VectorClock].lt(l._1, r._1)) -1
    else Ordering.by((v: (VectorClock, Instant, String)) => (v._2, v._3)).compare(l, r)
  }

  implicit def semiLattice: SemiLattice[LWWTime] = (l,r) =>
    if (l > r) l
    else if (l < r) r
    else if (l == r) l
    else throw new IllegalArgumentException(s"$l and $r can't be ordered")
}

object LastWriterWinsRegisterLattice {
  implicit def LWWRegLattice[T, O <: Ordered[O]]: SemiLattice[LastWriterWinsRegisterLattice[T, O]] = {
    (left, right) => {
      if (left == right) left
      else if (left.timestamp > right.timestamp) left
      else if (left.timestamp < right.timestamp) right
      else throw new IllegalArgumentException(s"$left and $right can't be ordered")
    }
  }

  implicit def LWWRegLattice[T, O](implicit ord: Ordering[O]): SemiLattice[LastWriterWinsRegisterLattice[T, O]] = {
    (left, right) => {
      val order = ord.compare(left.timestamp, right.timestamp)
      if (left == right) left
      else if (order > 0) left
      else if (order < 0) right
      else throw new IllegalArgumentException(s"$left and $right can't be ordered")
    }
  }
}