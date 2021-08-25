package de.ckuessner
package encrdt.lattices

import encrdt.lattices.interfaces.SemiLattice
import encrdt.util.MapHelper.max

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

/**
 * Counter CRDT using states
 */
class Counter(val replicaId: String) {

  def this(replicaId: String, initialState: CounterCrdtLattice) = {
    this(replicaId)
    this.state = initialState
  }

  // Local state of the CRDT
  private var _state = CounterCrdtLattice()

  def state: CounterCrdtLattice = _state

  private def state_=(state: CounterCrdtLattice): Unit = {
    _state = state
  }

  def merge(remoteState: CounterCrdtLattice): Unit = {
    state = SemiLattice.merged(state, remoteState)
  }

  // Local counts
  def query(): Int = state.query()

  def update(delta: Int): Unit = {
    state = state.updated(replicaId, delta)
  }
}


// Encapsulates the state of the CRDT
case class CounterCrdtLattice(positiveCounts: Map[String, Int] = Map(),
                              negativeCounts: Map[String, Int] = Map()) {

  def updated(replicaId: String, delta: Int): CounterCrdtLattice = {
    if (delta > 0) this.copy(
      positiveCounts = positiveCounts.updatedWith(replicaId)(value => Some(value.getOrElse(0) + delta))
    )
    else if (delta < 0) this.copy(
      negativeCounts = negativeCounts.updatedWith(replicaId)(value => Some(value.getOrElse(0) + delta.abs))
    )
    else this
  }

  def query(): Int = positiveCounts.values.sum - negativeCounts.values.sum
}

object CounterCrdtLattice {
  implicit val jsonCodec: JsonValueCodec[CounterCrdtLattice] = JsonCodecMaker.make

  implicit val semiLattice: SemiLattice[CounterCrdtLattice] = (left: CounterCrdtLattice, right: CounterCrdtLattice) =>
    CounterCrdtLattice(
      max(left.positiveCounts, right.positiveCounts),
      max(left.negativeCounts, right.negativeCounts)
    )
}
