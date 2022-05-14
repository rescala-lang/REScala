package kofre.datatypes

import kofre.base.{Bottom, DecomposeLattice}
import kofre.decompose.*
import kofre.dotted.DottedDecompose
import kofre.syntax.OpsSyntaxHelper

/** A TwoPSet (Two-Phase Set) is a Delta CRDT modeling a set.
  *
  * The set is modeled as two grow-only sets, a set of added elements and a set of removed elements. Because of this,
  * elements that were removed from the set once can never be re-added.
  */

case class TwoPSet[E](added: Set[E], removed: Set[E])

object TwoPSet {
  def empty[E]: TwoPSet[E] = TwoPSet(Set.empty, Set.empty)

  given bottom[E]: Bottom[TwoPSet[E]] with { override def empty: TwoPSet[E] = TwoPSet.empty }

  given decomposeLattice[E]: DecomposeLattice[TwoPSet[E]] = DecomposeLattice.derived
  given contextDecompose[E]: DottedDecompose[TwoPSet[E]]  = DottedDecompose.liftDecomposeLattice

  implicit class TwoPSetSyntax[C, E](container: C) extends OpsSyntaxHelper[C, TwoPSet[E]](container) {

    def elements(using QueryP): Set[E] = {
      current.added diff current.removed
    }

    def contains(element: E)(using QueryP): Boolean = current.added.contains(element) && !current.removed.contains(element)

    def insert(element: E)(using MutationP): C = TwoPSet(Set(element), Set.empty).mutator

    def remove(element: E)(using MutationP): C = TwoPSet(Set.empty, Set(element)).mutator
    def removeAll(elements: Set[E])(using MutationP): C = TwoPSet(Set.empty, elements).mutator
  }
}
