package kofre.decompose.interfaces

import kofre.datatypes.GrowMap
import kofre.decompose.*
import kofre.dotted.DottedDecompose
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper}

/** A GrowOnlySet is a Delta CRDT modeling a simple grow-only set. */
object GSetInterface {

  type GrowOnlySet[E] = Set[E]

  given contextDecompose[E]: DottedDecompose[GrowOnlySet[E]] = DottedDecompose.liftDecomposeLattice

  implicit class GSetSyntax[C, E](container: C) extends OpsSyntaxHelper[C, GrowOnlySet[E]](container) {

    def elements(using QueryP): Set[E] = current

    def insert(element: E)(using MutationP): C = Set(element).mutator
  }
}
