package kofre.dotted

import kofre.base.Lattice.Operators
import kofre.base.{Bottom, DecomposeLattice, Lattice, Id}
import kofre.dotted.{DotFun, DotSet}
import kofre.syntax.{ArdtOpsContains, DottedName, PermCausal, PermCausalMutate, PermQuery}
import kofre.time.{Dot, Dots}


case class Dotted[A](store: A, context: Dots) {
  def map[B](f: A => B): Dotted[B]                 = Dotted(f(store), context)
  def named(id: Id): DottedName[A] = DottedName(id, this)
}

/** Implicit aliases in companion object for search path */
object Dotted {

  def empty[A: Bottom]: Dotted[A] = Dotted(Bottom.empty[A], Dots.empty)
  def apply[A](a: A): Dotted[A]   = Dotted(a, Dots.empty)

  def latticeLift[L: DecomposeLattice: Bottom]: DecomposeLattice[Dotted[L]] = DecomposeLattice.derived


  given syntaxPermissions[L](using DottedLattice[L]): PermCausalMutate[Dotted[L], L]
    with {
    override def mutateContext(c: Dotted[L], delta: Dotted[L]): Dotted[L] = c merge delta
    override def query(c: Dotted[L]): L                                   = c.store
    override def context(c: Dotted[L]): Dots                              = c.context
  }

  given syntaxPassthroughTrans[K, L](using ArdtOpsContains[K, L]): ArdtOpsContains[Dotted[K], L] = new {}
  given syntaxPassthrough[L](using ArdtOpsContains[L, L]): ArdtOpsContains[Dotted[L], L]         = new {}
}
