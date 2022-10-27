package com.github.ckuessner.encrdt.crdts

import com.github.ckuessner.encrdt.crdts.interfaces.SetCrdt
import com.github.ckuessner.encrdt.lattices.{SemiLattice, TwoPhaseSetLattice}

class TwoPhaseSet[T](val replicaId: String) extends SetCrdt[T] {

  private var _state = TwoPhaseSetLattice[T]()

  def state: TwoPhaseSetLattice[T] = _state

  def merge(remoteState: TwoPhaseSetLattice[T]): Unit = {
    _state = SemiLattice[TwoPhaseSetLattice[T]].merged(state, remoteState)
  }

  def add(element: T): Unit = {
    _state = _state.added(element)
  }

  /**
   * Permanently removes the element from the Set.
   * When removing an element that is not currently present in the Set, the element can't be added later on.
   *
   * @param element The element to be removed
   */
  def remove(element: T): Unit = {
    _state = _state.removed(element)
  }

  def values: Set[T] = state.values
}
