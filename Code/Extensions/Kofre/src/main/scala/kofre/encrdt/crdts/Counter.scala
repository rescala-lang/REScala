
package kofre.encrdt.crdts
import kofre.Lattice

import kofre.encrdt.lattices.{CounterLattice}

/**
 * Counter CRDT using states
 */
class Counter(val replicaId: String) {

  def this(replicaId: String, initialState: CounterLattice) = {
    this(replicaId)
    this.state = initialState
  }

  // Local state of the CRDT
  private var _state = CounterLattice()

  def state: CounterLattice = _state

  private def state_=(state: CounterLattice): Unit = {
    _state = state
  }

  def merge(remoteState: CounterLattice): Unit = {
    state = Lattice.merge(state, remoteState)
  }

  // Local counts
  def query(): Int = state.query()

  def update(delta: Int): Unit = {
    state = state.updated(replicaId, delta)
  }
}
