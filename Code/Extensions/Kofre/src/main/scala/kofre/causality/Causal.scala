package kofre.causality

import kofre.Lattice
import kofre.causality.CausalContext
import kofre.dotbased.DotStore.{DotFun, DotMap, DotSet}
import kofre.causality.Causal
import kofre.dotbased.DotStore

case class Causal[A](store: A, context: CausalContext)



// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
object Causal {
  def bottom[D: DotStore]: Causal[D] = Causal(DotStore[D].empty, CausalContext.empty)

  // (s, c) ⨆ (s', c') = ((s ∩ s') ∪ (s \ c') ∪ (s' \ c), c ∪ c')
  implicit def CausalWithDotSetLattice: Lattice[Causal[DotSet]] = (left, right) => {
    val inBoth = left.store & right.store
    val newInLeft =
      left.store.filterNot(dot => dot.time <= right.context.clockOf(dot.replicaId).get.time)
    val newInRight =
      right.store.filterNot(dot => dot.time <= left.context.clockOf(dot.replicaId).get.time)

    val mergedCausalContext = left.context.union(right.context)
    Causal(inBoth ++ newInLeft ++ newInRight, mergedCausalContext)
  }

  // (m, c) ⨆ (m', c') = ( {k -> m(k) ⨆ m'(k) | k ∈ dom m ∩ dom m'} ∪
  //                       {(d, v) ∈ m  | d ∉ c'} ∪
  //                       {(d, v) ∈ m' | d ∉ c},
  //                      c ∪ c')
  implicit def CausalWithDotFunLattice[V: Lattice]: Lattice[Causal[DotFun[V]]] = (left, right) => {
    Causal(
      (left.store.keySet & right.store.keySet map { (dot: Dot) =>
        (dot, Lattice.merge(left.store(dot), right.store(dot)))
      }).toMap
      ++ left.store.filterNot { case (dot, _) => right.context.contains(dot) }
      ++ right.store.filterNot { case (dot, _) => left.context.contains(dot) },
      left.context.union(right.context)
      )
  }

  // (m, c) ⨆ (m', c') = ( {k -> v(k) | k ∈ dom m ∩ dom m' ∧ v(k) ≠ ⊥}, c ∪ c')
  //                      where v(k) = fst((m(k), c) ⨆ (m'(k), c'))
  implicit def CausalWithDotMapLattice[K, V: DotStore](implicit
                                                       vSemiLattice: Lattice[Causal[V]]
                                                      ): Lattice[Causal[DotMap[K, V]]] =
    (left: Causal[DotMap[K, V]], right: Causal[DotMap[K, V]]) =>
      Causal(
        ((left.store.keySet union right.store.keySet) map { key =>
          val leftCausal  = Causal(left.store.getOrElse(key, DotStore[V].empty), left.context)
          val rightCausal = Causal(right.store.getOrElse(key, DotStore[V].empty), right.context)
          key -> Lattice[Causal[V]].merge(leftCausal, rightCausal).store
        } filterNot {
           case (key, dotStore) => DotStore[V].empty == dotStore
         }).toMap,
        left.context.union(right.context)
        )
}
