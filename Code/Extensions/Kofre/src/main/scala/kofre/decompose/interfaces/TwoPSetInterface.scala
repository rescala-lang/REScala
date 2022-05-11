package kofre.decompose.interfaces

import kofre.decompose.*
import kofre.syntax.OpsSyntaxHelper

/** A TwoPSet (Two-Phase Set) is a Delta CRDT modeling a set.
  *
  * The set is modeled as two grow-only sets, a set of added elements and a set of removed elements. Because of this,
  * elements that were removed from the set once can never be re-added.
  */
object TwoPSetInterface {
  type TwoPSet[E] = (Set[E], Set[E])

  implicit class TwoPSetSyntax[C, E](container: C) extends OpsSyntaxHelper[C, TwoPSet[E]](container) {

    def elements(using QueryP): Set[E] = {
      val (add, remove) = current
      add diff remove
    }

    def insert(element: E)(using MutationP): C = (Set(element), Set.empty).mutator

    def remove(element: E)(using MutationP): C = (Set.empty, Set(element)).mutator
  }
}
