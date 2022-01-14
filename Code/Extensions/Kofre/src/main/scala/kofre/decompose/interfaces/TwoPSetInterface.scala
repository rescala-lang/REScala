package kofre.decompose.interfaces

import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose._

object TwoPSetInterface {
  type State[E] = (Set[E], Set[E])

  trait TwoPSetCompanion {
    type State[E] = TwoPSetInterface.State[E]
  }

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

/** A TwoPSet (Two-Phase Set) is a Delta CRDT modeling a set.
  *
  * The set is modeled as two grow-only sets, a set of added elements and a set of removed elements. Because of this,
  * elements that were removed from the set once can never be re-added.
  */
abstract class TwoPSetInterface[E, Wrapper] extends CRDTInterface[TwoPSetInterface.State[E], Wrapper] {
  def elements: Set[E] = query(TwoPSetInterface.elements)

  def insert(element: E): Wrapper = mutate(TwoPSetInterface.insert(element))

  def remove(element: E): Wrapper = mutate(TwoPSetInterface.remove(element))
}
