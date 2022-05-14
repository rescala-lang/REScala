package kofre.contextual

import kofre.base.{Bottom, DecomposeLattice, Lattice}
import kofre.time.{Dots, Dot}
import kofre.contextual.AsCausalContext
import kofre.base.Lattice.Operators
import kofre.dotted.{DotFun, DotSet}
import kofre.syntax.{ArdtOpsContains, PermCausal, PermCausalMutate, PermQuery, WithNamedContext}

import scala.util.NotGiven

case class Dotted[A](store: A, context: Dots) {
  def map[B](f: A => B): Dotted[B]                  = Dotted(f(store), context)
  def named(id: kofre.base.Defs.Id): WithNamedContext[A] = WithNamedContext(id, this)
}

/** Implicit aliases in companion object for search path */
object Dotted {

  def empty[A: Bottom]: Dotted[A] = Dotted(Bottom.empty[A], Dots.empty)
  def apply[A](a: A): Dotted[A]   = Dotted(a, Dots.empty)

  given bottomInstance[A: Bottom](using NotGiven[Bottom[Dotted[A]]]): Bottom[Dotted[A]] with {
    override def empty: Dotted[A] = Dotted.this.empty
  }

  given CausalWithDotFunLattice[V: Lattice]: Lattice[Dotted[DotFun[V]]] = kofre.dotted.DotFun.perDotLattice
  given CausalWithDotSetLattice: Lattice[Dotted[Set[Dot]]] =
    DotSet.contextLattice.bimap[Dotted[Set[Dot]]](
      _.map(_.repr.toSet),
      _.map(s => DotSet(Dots.fromSet(s)))
    )

  given latticeLift[L: DecomposeLattice]: DecomposeLattice[Dotted[L]] = DecomposeLattice.derived
  given syntaxPermissions[L](using ContextLattice[L]): PermCausalMutate[Dotted[L], L]
    with {
    override def mutateContext(c: Dotted[L], delta: Dotted[L]): Dotted[L] = c merged delta
    override def query(c: Dotted[L]): L                                             = c.store
    override def context(c: Dotted[L]): Dots                               = c.context
  }

  given syntaxPassthrough[L]: ArdtOpsContains[Dotted[L], L] = new ArdtOpsContains[Dotted[L], L] {}
}
