package com.github.ckuessner.ardt.datatypes

import com.github.ckuessner.ardt.base.Causal
import com.github.ckuessner.ardt.causality.{CausalContext, DotStore}
import com.github.ckuessner.ardt.causality.DotStore.{DotMap, DotSet}
import com.github.ckuessner.ardt.causality.impl.ArrayCausalContext

opaque type AddWinsSet[E] = Causal[DotMap[E, DotSet]]

extension [E](awSet: AddWinsSet[E])
  def elements: Set[E]              = awSet.dotStore.keySet
  def contains(element: E): Boolean = awSet.dotStore.contains(element)

object AddWinsSet:
  object mutators:
    /** Returns the '''delta''' that adds the element to the `set`.
      *
      * @param set
      *   Set that the element should be added to.
      * @param replicaId
      *   Id of the replica that performs the add.
      * @param element
      *   Element to add.
      * @tparam E
      *   Type of the elements in the set.
      * @return
      *   The delta of the add operation.
      */
    def add[E](set: AddWinsSet[E], replicaId: String, element: E): AddWinsSet[E] =
      val newDot                           = set.causalContext.clockOf(replicaId).advance(replicaId)
      val deltaDotStore: DotMap[E, DotSet] = Map(element -> ArrayCausalContext.single(newDot))
      val deltaCausalContext = CausalContext(
        set.dotStore.getOrElse(element, DotStore[DotSet].bottom).add(newDot.replicaId, newDot.time)
      )
      Causal(deltaDotStore, deltaCausalContext)

    /** Returns the '''delta''' that removes the element from the `set`.
      *
      * @param set
      *   The set to remove the element from
      * @param element
      *   Element to remove.
      * @tparam E
      *   Type of the elements in the set.
      * @return
      *   The delta of the removal operation.
      */
    def remove[E](set: AddWinsSet[E], element: E): AddWinsSet[E] = Causal(
      DotStore[DotMap[E, DotSet]].bottom,
      CausalContext(set.dotStore.getOrElse(element, DotStore[DotSet].bottom))
    )

    /** Returns the '''delta''' that removes all elements from the `set`.
      *
      * '''Doesn't return the full set, only the delta!'''
      *
      * @param set
      *   State before the removal of all elements
      * @tparam E
      *   Type of the elements in the set
      * @return
      *   The delta of the clear
      */
    def clear[E](set: AddWinsSet[E]): AddWinsSet[E] = Causal(
      DotStore[DotMap[E, DotSet]].bottom,
      CausalContext(DotStore[DotMap[E, DotSet]].dots(set.dotStore))
    )
