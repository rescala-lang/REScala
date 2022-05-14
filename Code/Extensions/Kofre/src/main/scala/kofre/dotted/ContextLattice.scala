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
trait ContextLattice[A] extends Lattice[Dotted[A]] {
  def mergePartial(left: Dotted[A], right: Dotted[A]): A

  def merge(left: Dotted[A], right: Dotted[A]): Dotted[A] =
    Dotted(
      mergePartial(left, right),
      left.context.merge(right.context)
    )

  extension (left: Dotted[A]) def conmerge(right: Dotted[A]): A = mergePartial(left, right)
}

object ContextLattice {
  def apply[A](using wcm: ContextLattice[A]): ContextLattice[A] = wcm

  given liftLattice[A: Lattice]: ContextLattice[A] = (left, right) => Lattice[A].merge(left.store, right.store)


  given pairPartialLattice[A: ContextLattice, B: ContextLattice]: ContextLattice[(A, B)] with {
    override def mergePartial(left: Dotted[(A, B)], right: Dotted[(A, B)]): (A, B) =
      val Dotted((left1, left2), leftCContext)    = left
      val Dotted((right1, right2), rightCContext) = right
      val stateMerged1                            = Dotted(left1, leftCContext) conmerge Dotted(right1, rightCContext)
      val stateMerged2                            = Dotted(left2, leftCContext) conmerge Dotted(right2, rightCContext)
      (stateMerged1, stateMerged2)
  }
}
