package kofre.contextual

import kofre.base.{Bottom, DecomposeLattice, Lattice}
import kofre.causality.{CausalContext, Dot}
import kofre.contextual.AsCausalContext
import kofre.base.Lattice.Operators
import kofre.dotted.{DotFun, DotSet}
import kofre.syntax.{ArdtOpsContains, PermCausal, PermCausalMutate, PermQuery, WithNamedContext}

import scala.util.NotGiven

case class WithContext[A](store: A, context: CausalContext) {
  def map[B](f: A => B): WithContext[B]                  = WithContext(f(store), context)
  def named(id: kofre.base.Defs.Id): WithNamedContext[A] = WithNamedContext(id, this)
}

/** Implicit aliases in companion object for search path */
object WithContext {

  def empty[A: Bottom]: WithContext[A] = WithContext(Bottom.empty[A], CausalContext.empty)
  def apply[A](a: A): WithContext[A]   = WithContext(a, CausalContext.empty)

  given bottomInstance[A: Bottom](using NotGiven[Bottom[WithContext[A]]]): Bottom[WithContext[A]] with {
    override def empty: WithContext[A] = WithContext.this.empty
  }

  given CausalWithDotFunLattice[V: Lattice]: Lattice[WithContext[DotFun[V]]] = kofre.dotted.DotFun.perDotLattice
  given CausalWithDotSetLattice: Lattice[WithContext[Set[Dot]]] =
    DotSet.contextLattice.bimap[WithContext[Set[Dot]]](
      _.map(_.repr.toSet),
      _.map(s => DotSet(CausalContext.fromSet(s)))
    )

  given latticeLift[L: DecomposeLattice]: DecomposeLattice[WithContext[L]] = DecomposeLattice.derived
  given syntaxPermissions[L](using ContextLattice[L]): PermCausalMutate[WithContext[L], L]
    with {
    override def mutateContext(c: WithContext[L], delta: WithContext[L]): WithContext[L] = c merged delta
    override def query(c: WithContext[L]): L                                             = c.store
    override def context(c: WithContext[L]): CausalContext                               = c.context
  }

  given syntaxPassthrough[L]: ArdtOpsContains[WithContext[L], L] = new ArdtOpsContains[WithContext[L], L] {}
}
