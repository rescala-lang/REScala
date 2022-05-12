package kofre.decompose.interfaces

import kofre.decompose.*
import kofre.decompose.interfaces.GMap
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper}

/** A GSet is a Delta CRDT modeling a simple grow-only set. */
object GSetInterface {

  type GSet[E] = Set[E]

  implicit class GSetSyntax[C, E](container: C) extends OpsSyntaxHelper[C, GSet[E]](container) {

    def elements(using QueryP): Set[E] = current

    def insert(element: E)(using MutationP): C = Set(element).mutator
  }
}
