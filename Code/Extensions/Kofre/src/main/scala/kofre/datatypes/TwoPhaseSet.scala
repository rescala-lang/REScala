package kofre.datatypes

import kofre.base.{Bottom, DecomposeLattice}
import kofre.dotted.DottedDecompose
import kofre.syntax.OpsSyntaxHelper

/** A TwoPhaseSet (Two-Phase Set) is a Delta CRDT modeling a set.
  *
  * The set is modeled as two grow-only sets, a set of added elements and a set of removed elements. Because of this,
  * elements that were removed from the set once can never be re-added.
  */

case class TwoPhaseSet[E](added: Set[E], removed: Set[E])

object TwoPhaseSet {
  def empty[E]: TwoPhaseSet[E] = TwoPhaseSet(Set.empty, Set.empty)

  given bottom[E]: Bottom[TwoPhaseSet[E]] with { override def empty: TwoPhaseSet[E] = TwoPhaseSet.empty }

  given decomposeLattice[E]: DecomposeLattice[TwoPhaseSet[E]] = DecomposeLattice.derived
  given contextDecompose[E]: DottedDecompose[TwoPhaseSet[E]]  = DottedDecompose.liftLattice

  extension [C, E](container: C)
    def twoPhaseSet: syntax[C, E] = syntax(container)

  implicit class syntax[C, E](container: C) extends OpsSyntaxHelper[C, TwoPhaseSet[E]](container) {

    def elements(using PermQuery): Set[E] = {
      current.added diff current.removed
    }

    def contains(using PermQuery)(element: E): Boolean =
      current.added.contains(element) && !current.removed.contains(element)

    def insert(using PermMutate)(element: E): C = TwoPhaseSet(Set(element), Set.empty).mutator

    def remove(using PermMutate)(element: E): C          = TwoPhaseSet(Set.empty, Set(element)).mutator
    def removeAll(using PermMutate)(elements: Set[E]): C = TwoPhaseSet(Set.empty, elements).mutator
  }
}
