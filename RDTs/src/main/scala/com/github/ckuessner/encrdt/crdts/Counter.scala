package com.github.ckuessner.encrdt.crdts

import com.github.ckuessner.encrdt.lattices.{CounterLattice, SemiLattice}

/** Counter CRDT using states
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
    state = SemiLattice.merged(state, remoteState)
  }

  // Local counts
  def query(): Int = state.query()

  def update(delta: Int): Unit = {
    state = state.updated(replicaId, delta)
  }
}
