package kofre.dotbased

import kofre.Lattice
import kofre.causality.{CausalContext, Dot}
import kofre.dotbased.AsCausalContext
import kofre.Lattice.Operators
import kofre.decompose.{DecomposableDotStore, UIJDLattice}

case class WithContext[A](store: A, context: CausalContext)

object WithContext {

  given latticeInContext[A: WithContextMerge]: Lattice[WithContext[A]] = (left, right) =>
    WithContext(
      WithContextMerge[A].mergePartial(left, right),
      left.context.merge(right.context)
    )

  given CausalWithDotFunLattice[V: Lattice]: Lattice[WithContext[Map[Dot, V]]] = latticeInContext

  /** Alias for better automatic inference */
  given CausalWithDotSetLattice: Lattice[WithContext[Set[Dot]]] = UIJDLattice.contextUIJDLattice

  /** Alias for better automatic inference */
  given CausalWithContextSetLattice: Lattice[WithContext[CausalContext]] = UIJDLattice.contextUIJDLattice

  // (m, c) ⨆ (m', c') = ( {k -> v(k) | k ∈ dom m ∩ dom m' ∧ v(k) ≠ ⊥}, c ∪ c')
  //                      where v(k) = fst((m(k), c) ⨆ (m'(k), c'))
  implicit def CausalWithDotMapLattice[K, V: AsCausalContext](implicit
      vlattice: Lattice[WithContext[V]]
  ): Lattice[WithContext[Map[K, V]]] =
    (left: WithContext[Map[K, V]], right: WithContext[Map[K, V]]) =>
      WithContext(
        ((left.store.keySet union right.store.keySet) map { key =>
          val leftCausalStore  = WithContext(left.store.getOrElse(key, AsCausalContext[V].empty), left.context)
          val rightCausalStore = WithContext(right.store.getOrElse(key, AsCausalContext[V].empty), right.context)
          key -> Lattice.merge(leftCausalStore, rightCausalStore).store
        } filterNot {
          case (key, store) => AsCausalContext[V].empty == store
        }).toMap,
        left.context.union(right.context)
      )
}
