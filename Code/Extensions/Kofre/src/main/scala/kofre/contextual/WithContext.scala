package kofre.contextual

import kofre.base.{Bottom, DecomposeLattice, Lattice}
import kofre.causality.{CausalContext, Dot}
import kofre.contextual.AsCausalContext
import kofre.base.Lattice.Operators
import kofre.syntax.{ArdtOpsContains, PermCausal, PermCausalMutate, PermQuery}

case class WithContext[A](store: A, context: CausalContext) {
  def map[B](f: A => B): WithContext[B] = WithContext(f(store), context)
}

/** Implicit aliases in companion object for search path */
object WithContext {

  def empty[A: Bottom]: WithContext[A] = WithContext(Bottom.empty[A], CausalContext.empty)

  given CausalWithDotFunLattice[V: Lattice]: Lattice[WithContext[Map[Dot, V]]] = ContextLattice.perDot
  given CausalWithDotSetLattice: Lattice[WithContext[Set[Dot]]] =
    ContextLattice.causalContext.bimap[WithContext[Set[Dot]]](
      { case WithContext(s, c) => WithContext(s.toSet, c) },
      { case WithContext(s, c) => WithContext(CausalContext.fromSet(s), c) }
    )
  given CausalWithContextSetLattice: Lattice[WithContext[CausalContext]] = ContextLattice.causalContext
  given CausalWithDotMapLattice[K, V: AsCausalContext: ContextLattice]: Lattice[WithContext[Map[K, V]]] =
    ContextLattice.dotMapLattice

  given syntaxPermissions[L](using DecomposeLattice[WithContext[L]]): PermQuery[WithContext[L], L]
    with PermCausal[WithContext[L]] with PermCausalMutate[WithContext[L], L]
    with {
    override def mutateContext(c: WithContext[L], delta: WithContext[L]): WithContext[L] = c merged delta
    override def query(c: WithContext[L]): L                                             = c.store
    override def context(c: WithContext[L]): CausalContext                               = c.context
  }

  given syntaxPassthrough[L]: ArdtOpsContains[WithContext[L], L] = new ArdtOpsContains[WithContext[L], L] {}
}
