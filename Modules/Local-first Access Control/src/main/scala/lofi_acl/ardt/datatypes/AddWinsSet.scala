package lofi_acl.ardt.datatypes

import lofi_acl.ardt.base.Causal
import rdts.base.Bottom
import rdts.dotted.HasDots
import rdts.syntax.LocalUid
import rdts.time.Dots

opaque type AddWinsSet[E] = Causal[Map[E, Dots]]

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
    def add[E](set: AddWinsSet[E], replicaId: LocalUid, element: E): AddWinsSet[E] =
      val newDot                           = set.causalContext.nextDot(replicaId.uid)
      val deltaDotStore: Map[E, Dots] = Map(element -> Dots.single(newDot))
      val deltaCausalContext = set.dotStore.get(element) match
        case Some(dots) => dots.add(newDot)
        case None       => Dots.single(newDot)
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
      Map.empty,
      set.dotStore.getOrElse(element, Dots.empty)
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
      Bottom[Map[E, Dots]].empty,
      HasDots[Map[E, Dots]].dots(set.dotStore)
    )
