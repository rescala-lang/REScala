package lofi_acl.ardt.datatypes

import rdts.base.Bottom
import rdts.dotted.{Dotted, HasDots}
import rdts.syntax.LocalUid
import rdts.time.Dots

opaque type AddWinsSet[E] = Dotted[Map[E, Dots]]

extension [E](awSet: AddWinsSet[E])
  def elements: Set[E]              = awSet.data.keySet
  def contains(element: E): Boolean = awSet.data.contains(element)

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
      val newDot                           = set.context.nextDot(replicaId.uid)
      val deltaDotStore: Map[E, Dots] = Map(element -> Dots.single(newDot))
      val deltaCausalContext = set.data.get(element) match
        case Some(dots) => dots.add(newDot)
        case None       => Dots.single(newDot)
      Dotted(deltaDotStore, deltaCausalContext)

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
    def remove[E](set: AddWinsSet[E], element: E): AddWinsSet[E] = Dotted(
      Map.empty,
      set.data.getOrElse(element, Dots.empty)
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
    def clear[E](set: AddWinsSet[E]): AddWinsSet[E] = Dotted(
      Bottom[Map[E, Dots]].empty,
      HasDots[Map[E, Dots]].dots(set.data)
    )
