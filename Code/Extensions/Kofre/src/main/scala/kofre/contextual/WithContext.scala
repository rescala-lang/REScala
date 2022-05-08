package kofre.contextual

import kofre.base.{DecomposeLattice, Lattice}
import kofre.causality.{CausalContext, Dot}
import kofre.contextual.AsCausalContext
import kofre.base.Lattice.Operators
import kofre.syntax.{ArdtOpsContains, PermCausal, PermCausalMutate, PermQuery}

case class WithContext[A](store: A, context: CausalContext)

/** Implicit aliases in companion object for search path */
object WithContext {
  given CausalWithDotFunLattice[V: Lattice]: Lattice[WithContext[Map[Dot, V]]] = WithContextMerge.perDot
  given CausalWithDotSetLattice: Lattice[WithContext[Set[Dot]]]                = DecomposeLattice.contextUIJDLattice
  given CausalWithContextSetLattice: Lattice[WithContext[CausalContext]]       = DecomposeLattice.contextUIJDLattice
  given CausalWithDotMapLattice[K, V: AsCausalContext: WithContextMerge]: Lattice[WithContext[Map[K, V]]] =
    WithContextMerge.dotMapMerge

  given syntaxPermissions[L](using DecomposeLattice[WithContext[L]]): PermQuery[WithContext[L], L]
    with PermCausal[WithContext[L]] with PermCausalMutate[WithContext[L], L]
    with {
    override def mutateContext(c: WithContext[L], delta: WithContext[L]): WithContext[L] = c merged delta
    override def query(c: WithContext[L]): L                                             = c.store
    override def context(c: WithContext[L]): CausalContext                               = c.context
  }

  given syntaxPassthrough[L]: ArdtOpsContains[WithContext[L], L] = new ArdtOpsContains[WithContext[L], L] {}
}
