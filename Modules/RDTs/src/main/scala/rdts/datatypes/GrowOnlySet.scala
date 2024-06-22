package rdts.datatypes

import rdts.base.{Bottom, Lattice}
import rdts.syntax.OpsSyntaxHelper

type GrowOnlySet[E] = Set[E]

/** A GrowOnlySet is a Delta CRDT modeling a simple grow-only set. */
object GrowOnlySet {

  extension [E](set: GrowOnlySet[E]) {
    def insert(e: E): GrowOnlySet[E] = Set(e)
    def elements: GrowOnlySet[E] = set
  }

  def empty[E]: GrowOnlySet[E] = Set.empty

  given bottomInstance[E]: Bottom[GrowOnlySet[E]] = Bottom.setBottom
  given lattice[E]: Lattice[GrowOnlySet[E]]       = Lattice.setLattice

}
