package com.github.ckuessner.ardt.base

import com.github.ckuessner.ardt.causality.DotStore.{DotFun, DotMap, DotSet}
import com.github.ckuessner.ardt.causality.{CausalContext, Dot, DotStore}

case class Causal[D](dotStore: D, causalContext: CausalContext)

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
object Causal {
  def bottom[D: DotStore]: Causal[D] = Causal(DotStore[D].bottom, CausalContext())

  // (s, c) ⨆ (s', c') = ((s ∩ s') ∪ (s \ c') ∪ (s' \ c), c ∪ c')
  given CausalWithDotSetLattice: Lattice[Causal[DotSet]] = (left, right) => {
    val inBoth     = left.dotStore `intersect` right.dotStore
    val newInLeft  = left.dotStore `subtract` right.causalContext.acc
    val newInRight = right.dotStore `subtract` left.causalContext.acc

    val mergedCausalContext = left.causalContext.merged(right.causalContext)
    Causal(inBoth `union` newInLeft `union` newInRight, mergedCausalContext)
  }

  // (m, c) ⨆ (m', c') = ( {k -> m(k) ⨆ m'(k) | k ∈ dom m ∩ dom m'} ∪
  //                       {(d, v) ∈ m  | d ∉ c'} ∪
  //                       {(d, v) ∈ m' | d ∉ c},
  //                      c ∪ c')
  given CausalWithDotFunLattice[V: Lattice]: Lattice[Causal[DotFun[V]]] = (left, right) => {
    Causal(
      ((left.dotStore.keySet `intersect` right.dotStore.keySet) map { (dot: Dot) =>
        (dot, Lattice.merge(left.dotStore(dot), right.dotStore(dot)))
      }).toMap
      ++ left.dotStore.filterNot { case (dot, _) => right.causalContext.contains(dot) }
      ++ right.dotStore.filterNot { case (dot, _) => left.causalContext.contains(dot) },
      left.causalContext.merged(right.causalContext)
    )
  }

  // (m, c) ⨆ (m', c') = ( {k -> v(k) | k ∈ dom m ∩ dom m' ∧ v(k) ≠ ⊥}, c ∪ c')
  //                      where v(k) = fst((m(k), c) ⨆ (m'(k), c'))
  given CausalWithDotMapLattice[K, V: DotStore](using Lattice[Causal[V]]): Lattice[Causal[DotMap[K, V]]] =
    (left: Causal[DotMap[K, V]], right: Causal[DotMap[K, V]]) =>
      Causal(
        ((left.dotStore.keySet union right.dotStore.keySet) map { key =>
          val leftCausal  = Causal(left.dotStore.getOrElse(key, DotStore[V].bottom), left.causalContext)
          val rightCausal = Causal(right.dotStore.getOrElse(key, DotStore[V].bottom), right.causalContext)
          key -> Lattice[Causal[V]].merge(leftCausal, rightCausal).dotStore
        } filterNot { case (key, dotStore) =>
          DotStore[V].bottom == dotStore
        }).toMap,
        left.causalContext.merged(right.causalContext)
      )
}
