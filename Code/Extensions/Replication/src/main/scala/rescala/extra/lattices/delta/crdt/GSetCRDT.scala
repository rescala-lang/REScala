package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta._

object GSetCRDT {
  type State[E] = Set[E]

  def elements[E]: DeltaQuery[State[E], Set[E]] = state => state

  def insert[E](element: E): DeltaMutator[State[E]] = (_, _) => Set(element)
}
