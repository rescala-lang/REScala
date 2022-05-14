package kofre.contextual

import kofre.base.{DecomposeLattice, Lattice}
import kofre.causality.{CausalContext, Dot}
import kofre.contextual.AsCausalContext
import kofre.base.Lattice.Operators
import kofre.base.Bottom

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
trait ContextLattice[A] extends Lattice[WithContext[A]] {
  def mergePartial(left: WithContext[A], right: WithContext[A]): A

  def merge(left: WithContext[A], right: WithContext[A]): WithContext[A] =
    WithContext(
      mergePartial(left, right),
      left.context.merge(right.context)
    )

  extension (left: WithContext[A]) def conmerge(right: WithContext[A]): A = mergePartial(left, right)
}

object ContextLattice {
  def apply[A](using wcm: ContextLattice[A]): ContextLattice[A] = wcm

  given liftLattice[A: Lattice]: ContextLattice[A] = (left, right) => Lattice[A].merge(left.store, right.store)


  given pairPartialLattice[A: ContextLattice, B: ContextLattice]: ContextLattice[(A, B)] with {
    override def mergePartial(left: WithContext[(A, B)], right: WithContext[(A, B)]): (A, B) =
      val WithContext((left1, left2), leftCContext)    = left
      val WithContext((right1, right2), rightCContext) = right
      val stateMerged1 = WithContext(left1, leftCContext) conmerge WithContext(right1, rightCContext)
      val stateMerged2 = WithContext(left2, leftCContext) conmerge WithContext(right2, rightCContext)
      (stateMerged1, stateMerged2)
  }
}
