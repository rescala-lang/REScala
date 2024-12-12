package rdts.datatypes

import rdts.base.{Bottom, Decompose, Lattice}
import rdts.dotted.HasDots

/** A TwoPhaseSet (Two-Phase Set) is a Delta CRDT modeling a set.
  *
  * The set is modeled as two grow-only sets, a set of added elements and a set of removed elements. Because of this,
  * elements that were removed from the set once can never be re-added.
  */

case class TwoPhaseSet[E](added: Set[E], removed: Set[E]) {

  type Delta = TwoPhaseSet[E]

  def elements: Set[E] = {
    added `diff` removed
  }

  def contains(element: E): Boolean =
    added.contains(element) && !removed.contains(element)

  def insert(element: E): Delta = TwoPhaseSet(Set(element), Set.empty)

  def remove(element: E): Delta = TwoPhaseSet(Set.empty, Set(element))

  def removeAll(elements: Set[E]): Delta = TwoPhaseSet(Set.empty, elements)
}

object TwoPhaseSet {
  def empty[E]: TwoPhaseSet[E] = TwoPhaseSet(Set.empty, Set.empty)

  given bottom[E]: Bottom[TwoPhaseSet[E]] with { override def empty: TwoPhaseSet[E] = TwoPhaseSet.empty }

  given lattice[E]: Lattice[TwoPhaseSet[E]]     = Lattice.derived
  given decompose[E]: Decompose[TwoPhaseSet[E]] = Decompose.derived
  given hasDots[E]: HasDots[TwoPhaseSet[E]]     = HasDots.noDots

}
