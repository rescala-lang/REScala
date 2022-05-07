package kofre.contextual

import kofre.Lattice
import kofre.causality.{CausalContext, Dot}
import kofre.contextual.AsCausalContext
import kofre.Lattice.Operators
import kofre.decompose.DecomposeLattice

case class WithContext[A](store: A, context: CausalContext)

/** Implicit aliases in companion object for search path */
object WithContext {
  given CausalWithDotFunLattice[V: Lattice]: Lattice[WithContext[Map[Dot, V]]] = WithContextMerge.perDot
  given CausalWithDotSetLattice: Lattice[WithContext[Set[Dot]]]                = DecomposeLattice.contextUIJDLattice
  given CausalWithContextSetLattice: Lattice[WithContext[CausalContext]]       = DecomposeLattice.contextUIJDLattice
  given CausalWithDotMapLattice[K, V: AsCausalContext: WithContextMerge]: Lattice[WithContext[Map[K, V]]] =
    WithContextMerge.dotMapMerge
}
