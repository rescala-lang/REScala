package rdts.datatypes

import rdts.base.{Bottom, Lattice}
import rdts.dotted.HasDots
import rdts.syntax.OpsSyntaxHelper

/** A TwoPhaseSet (Two-Phase Set) is a Delta CRDT modeling a set.
  *
  * The set is modeled as two grow-only sets, a set of added elements and a set of removed elements. Because of this,
  * elements that were removed from the set once can never be re-added.
  */

case class TwoPhaseSet[E](added: Set[E], removed: Set[E])

object TwoPhaseSet {
  def empty[E]: TwoPhaseSet[E] = TwoPhaseSet(Set.empty, Set.empty)

  given bottom[E]: Bottom[TwoPhaseSet[E]] with { override def empty: TwoPhaseSet[E] = TwoPhaseSet.empty }

  given lattice[E]: Lattice[TwoPhaseSet[E]] = Lattice.derived
  given hasDots[E]: HasDots[TwoPhaseSet[E]] = HasDots.noDots

  extension [C, E](container: C)
    def twoPhaseSet: syntax[C, E] = syntax(container)

  implicit class syntax[C, E](container: C) extends OpsSyntaxHelper[C, TwoPhaseSet[E]](container) {

    def elements(using IsQuery): Set[E] = {
      current.added diff current.removed
    }

    def contains(using IsQuery)(element: E): Boolean =
      current.added.contains(element) && !current.removed.contains(element)

    def insert(using IsMutator)(element: E): C = TwoPhaseSet(Set(element), Set.empty).mutator

    def remove(using IsMutator)(element: E): C          = TwoPhaseSet(Set.empty, Set(element)).mutator
    def removeAll(using IsMutator)(elements: Set[E]): C = TwoPhaseSet(Set.empty, elements).mutator
  }
}
