package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta._

object TwoPSetCRDT {
  type State[E] = (Set[E], Set[E])

  private def deltaState[E](
      add: Set[E] = Set.empty[E],
      remove: Set[E] = Set.empty[E]
  ): State[E] = (add, remove)

  def elements[E]: DeltaQuery[State[E], Set[E]] = {
    case (add, remove) => add diff remove
  }

  def insert[E](element: E): DeltaMutator[State[E]] = (_, _) => deltaState(add = Set(element))

  def remove[E](element: E): DeltaMutator[State[E]] = (_, _) => deltaState(remove = Set(element))
}
