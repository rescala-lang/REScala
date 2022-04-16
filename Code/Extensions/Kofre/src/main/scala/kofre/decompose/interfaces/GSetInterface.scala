package kofre.decompose.interfaces

import kofre.decompose.*
import kofre.decompose.interfaces.GMapInterface.GMap
import kofre.syntax.{ArdtOpsContains, DeltaMutator, DeltaQuery, OpsSyntaxHelper}

/** A GSet is a Delta CRDT modeling a simple grow-only set. */
object GSetInterface {

  implicit class GSetSyntax[C, E](container: C) extends OpsSyntaxHelper[C, Set[E]](container) {

    def elements(using QueryP): Set[E] = current

    def insert(element: E)(using MutationP): C = Set(element)
  }
}
