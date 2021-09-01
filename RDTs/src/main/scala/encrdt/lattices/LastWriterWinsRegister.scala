package de.ckuessner
package encrdt.lattices

import encrdt.causality.VectorClock
import encrdt.lattices.interfaces.SemiLattice

import java.time.Instant
import scala.math.PartialOrdering

class LastWriterWinsRegister[T](initialState: LastWriterWinsRegisterLattice[T, (VectorClock, Instant, String)],
                                val replicaId: String) {

  import LastWriterWinsRegisterLattice.causallyConsistentTimeReplicaOrd // SemiLattice requires ordering of timestamp

  private var _state = initialState

  def currentTime: VectorClock = state.timestamp._1

  def state: LastWriterWinsRegisterLattice[T, (VectorClock, Instant, String)] = _state

  def value: T = state.value

  def set(value: T): Unit = {
    _state = SemiLattice.merged(
      state,
      LastWriterWinsRegisterLattice(value, (currentTime.advance(replicaId), Instant.now(), replicaId))
    )
  }

  def merge(otherState: LastWriterWinsRegisterLattice[T, (VectorClock, Instant, String)]): Unit =
    _state = SemiLattice.merged(this.state, otherState)
}

object LastWriterWinsRegister {
  def apply[T](replicaId: String, initialValue: T): LastWriterWinsRegister[T] =
    new LastWriterWinsRegister(
      LastWriterWinsRegisterLattice(
        initialValue,
        (VectorClock().advance(replicaId), Instant.now(), replicaId)),
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

object LastWriterWinsRegisterLattice {
  implicit def causallyConsistentTimeReplicaOrd: Ordering[(VectorClock, Instant, String)] = (l, r) => {
    if (PartialOrdering[VectorClock].gt(l._1, r._1)) 1
    else if (PartialOrdering[VectorClock].lt(l._1, r._1)) -1
    else Ordering.by((v: (VectorClock, Instant, String)) => (v._2, v._3)).compare(l, r)
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