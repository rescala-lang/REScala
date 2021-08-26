package de.ckuessner
package encrdt.lattices

import encrdt.lattices.interfaces.{MapCrdt, SemiLattice}

class TwoPhaseMap[K, V: SemiLattice](val replicaId: String) extends MapCrdt[K, V] {

  private var _state = TwoPhaseMapLattice[K, V]()

  def state: TwoPhaseMapLattice[K, V] = _state

  override def get(key: K): Option[V] = _state.get(key)

  override def put(key: K, value: V): Unit = _state = _state.added(key, value)

  override def remove(key: K): Unit = _state = _state.removed(key)

  override def values: Map[K, V] = _state.values
}

case class TwoPhaseMapLattice[K, V: SemiLattice](entries: TwoPhaseSetLattice[(K, V)] = TwoPhaseSetLattice()) {
  def keys: Set[K] = entries.values.map(_._1)

  def values: Map[K, V] = entries.values.toMap

  def get(key: K): Option[V] = entries.values.find(_._1 == key).map(_._2)

  def added(key: K, value: V): TwoPhaseMapLattice[K, V] = {
    // Was key previously removed? If yes, adding it won't change the outcome
    if (entries.removedElems.exists(_._1 == key)) return this
    // Is there a conflicting value?
    // There should be at most one (at least if key->??? ist not in entries.removed)
    get(key) match {
      case Some(conflictingValue) =>
        val mergedValue = SemiLattice.merged(value, conflictingValue) // Least upper bound of value
        val mergedAdded = entries.addedElems.filter(_._1 != key) + (key -> mergedValue) // addedElems with merged k-v-pair
        TwoPhaseMapLattice(entries.copy(addedElems = mergedAdded))
      case None =>
        TwoPhaseMapLattice(entries.added(key -> value))
    }
  }

  def removed(key: K): TwoPhaseMapLattice[K, V] = {
    TwoPhaseMapLattice(TwoPhaseSetLattice(entries.addedElems, entries.removedElems + (key -> get(key))))
  }
}