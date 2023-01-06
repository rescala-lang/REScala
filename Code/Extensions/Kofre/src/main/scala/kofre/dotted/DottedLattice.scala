package kofre.dotted

import kofre.base.Lattice.Operators
import kofre.base.{Bottom, DecomposeLattice, Lattice}
import kofre.time.{Dot, Dots}

import scala.annotation.targetName

/** The delta CRDT paper introduces a lot of individual abstractions
  * that all do a lot of cool stuff, but often not separated into their pieces.
  * This is one of those pieces systematically handling removals based on metadata.
  *
  * The context encodes dots that have been seen.
  * The store of type [[A]] is assumed to be something that stores individual information per dot.
  * We represent removals as context that contains a dot, but a store that no longer contains the dot.
  * Thus, when merging the abstract logic is somewhat along the lines of:
  *   Both contain the (dot, value) pair? => merge value recursively.
  *   Only one contains the dot in the context? => keep that value.
  *   Both contain the dot, but at least one has no matching value? => remove all values for that dot.
  *
  * Separating into a [[mergePartial]] allows extracting the context into the outermost layer reducing metadata overhead.
  */
trait DottedLattice[A] extends Lattice[Dotted[A]] {

  /** Partial merging combines the stored values, but ignores the context.
    * Thus enabling nested merging of values, without merging context multiple times.
    */
  def mergePartial(left: Dotted[A], right: Dotted[A]): A

  def merge(left: Dotted[A], right: Dotted[A]): Dotted[A] =
    Dotted(
      mergePartial(left, right),
      left.context.merge(right.context)
    )

  override def lteq(left: Dotted[A], right: Dotted[A]): Boolean =
    if !(left.context <= right.context) then false
    else super.lteq(left, right)

  extension (left: Dotted[A])
    @targetName("mergePartialExtension")
    def mergePartial(right: Dotted[A]): A = this.mergePartial(left, right)
}

object DottedLattice {
  def fromLattice[A: Lattice]: DottedLattice[A] = new DottedLattice[A]:
    override def mergePartial(left: Dotted[A], right: Dotted[A]): A = left.store merge right.store
}
