package kofre.datatypes

import kofre.base.{Bottom, Lattice}
import kofre.datatypes.GrowOnlyMap
import kofre.dotted.DottedLattice
import kofre.syntax.{OpsSyntaxHelper}

type GrowOnlySet[E] = Set[E]

/** A GrowOnlySet is a Delta CRDT modeling a simple grow-only set. */
object GrowOnlySet {

  def empty[E]: GrowOnlySet[E] = Set.empty

  given bottomInstance[E]: Bottom[GrowOnlySet[E]]          = Bottom.setBottom
  given lattice[E]: Lattice[GrowOnlySet[E]]                = Lattice.setLattice
  given contextDecompose[E]: DottedLattice[GrowOnlySet[E]] = DottedLattice.liftLattice

  extension [C, E](container: C)
    def growOnlySet: syntax[C, E] = syntax(container)

  implicit class syntax[C, E](container: C) extends OpsSyntaxHelper[C, GrowOnlySet[E]](container) {

    def elements(using PermQuery): Set[E] = current

    def insert(element: E)(using PermMutate): C = Set(element).mutator
  }
}
