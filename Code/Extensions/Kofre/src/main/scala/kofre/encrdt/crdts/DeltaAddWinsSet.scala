package kofre.encrdt.crdts

import kofre.Lattice
import kofre.dotbased.DotStore
import kofre.dotbased.DotStore.{DotMap, DotSet}
import kofre.causality.{CausalContext, Causal}
import kofre.encrdt.crdts.DeltaAddWinsSet.DeltaAddWinsSetLattice
import kofre.causality.Causal

class DeltaAddWinsSet[E](
    val replicaId: String,
    initialState: DeltaAddWinsSetLattice[E] = Causal.bottom[DotMap[E, DotSet]]
) {

  private var _state: DeltaAddWinsSetLattice[E]         = initialState
  private var deltas: Vector[DeltaAddWinsSetLattice[E]] = Vector()

  def state: DeltaAddWinsSetLattice[E] = _state

  def add(element: E): Unit =
    mutate(DeltaAddWinsSet.deltaAdd(replicaId, element, _state))

  def remove(element: E): Unit =
    mutate(DeltaAddWinsSet.deltaRemove(element, _state))

  def values: Set[E] =
    _state.store.keySet

  private def mutate(delta: DeltaAddWinsSetLattice[E]): Unit = {
    deltas = deltas.appended(delta)
    _state = Lattice[DeltaAddWinsSetLattice[E]].merge(_state, delta)
    // TODO: This is the place to hook in anti entropy
  }
}

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
object DeltaAddWinsSet {
  type DeltaAddWinsSetLattice[E] = Causal[DotMap[E, DotSet]]

  /** Returns the '''delta''' that adds the element to the `set`.
    *
    * '''Doesn't return the full set, only the delta!'''
    *
    * @param replicaId Id of the replica that performs the add
    * @param element   Element to add
    * @param set       State before the add
    * @tparam E Type of the elements in the set
    * @return The delta of the add
    */
  def deltaAdd[E](replicaId: String, element: E, set: DeltaAddWinsSetLattice[E]): DeltaAddWinsSetLattice[E] = {

    val newDot                           = set.context.clockOf(replicaId).get.advance
    val deltaDotStore: DotMap[E, DotSet] = Map(element -> Set(newDot))
    val deltaCausalContext = CausalContext.fromSet(set.store.getOrElse(element, DotStore[DotSet].empty) + newDot)
    Causal(deltaDotStore, deltaCausalContext)
  }

  /** Returns the '''delta''' that removes the element from the `set`.
    *
    * '''Doesn't return the full set, only the delta!'''
    *
    * @param element Element to remove
    * @param set     State before the remove
    * @tparam E Type of the elements in the set
    * @return The delta of the remove
    */
  def deltaRemove[E](element: E, set: DeltaAddWinsSetLattice[E]): DeltaAddWinsSetLattice[E] = Causal(
    DotStore[DotMap[E, DotSet]].empty,
    CausalContext.fromSet(set.store.getOrElse(element, DotStore[DotSet].empty))
    )

  /** Returns the '''delta''' that removes all elements from the `set`.
    *
    * '''Doesn't return the full set, only the delta!'''
    *
    * @param set State before the removal of all elements
    * @tparam E Type of the elements in the set
    * @return The delta of the clear
    */
  def deltaClear[E](set: DeltaAddWinsSetLattice[E]): DeltaAddWinsSetLattice[E] = Causal(
    DotStore[DotMap[E, DotSet]].empty,
    CausalContext.fromSet(DotStore[DotMap[E, DotSet]].dots(set.store))
    )
}
