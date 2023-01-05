package kofre.datatypes

import kofre.datatypes.GrowOnlyMap
import kofre.decompose.*
import kofre.dotted.DottedDecompose
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper}

type GrowOnlySet[E] = Set[E]

/** A GrowOnlySet is a Delta CRDT modeling a simple grow-only set. */
object GrowOnlySet {

  given contextDecompose[E]: DottedDecompose[GrowOnlySet[E]] = DottedDecompose.liftDecomposeLattice

  implicit class syntax[C, E](container: C) extends OpsSyntaxHelper[C, GrowOnlySet[E]](container) {

    def elements(using QueryP): Set[E] = current

    def insert(element: E)(using MutationP): C = Set(element).mutator
  }
}
