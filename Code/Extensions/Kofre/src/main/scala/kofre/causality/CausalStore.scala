package kofre.causality

import kofre.Lattice
import kofre.causality.CausalContext
import kofre.dotbased.DotStore.{DotFun, DotMap, DotSet}
import kofre.causality.CausalStore
import kofre.dotbased.DotStore

case class CausalStore[A](store: A, context: CausalContext)



// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
object CausalStore {
  def bottom[D: DotStore]: CausalStore[D] = CausalStore(DotStore[D].empty, CausalContext.empty)

  // (s, c) ⨆ (s', c') = ((s ∩ s') ∪ (s \ c') ∪ (s' \ c), c ∪ c')
  implicit def CausalWithDotSetLattice: Lattice[CausalStore[DotSet]] = (left, right) => {
    val inBoth = left.store & right.store
    val newInLeft =
      left.store.filterNot(dot => dot.time <= right.context.clockOf(dot.replicaId).get.time)
    val newInRight =
      right.store.filterNot(dot => dot.time <= left.context.clockOf(dot.replicaId).get.time)

    val mergedCausalContext = left.context.union(right.context)
    CausalStore(inBoth ++ newInLeft ++ newInRight, mergedCausalContext)
  }

  // (m, c) ⨆ (m', c') = ( {k -> m(k) ⨆ m'(k) | k ∈ dom m ∩ dom m'} ∪
  //                       {(d, v) ∈ m  | d ∉ c'} ∪
  //                       {(d, v) ∈ m' | d ∉ c},
  //                      c ∪ c')
  implicit def CausalWithDotFunLattice[V: Lattice]: Lattice[CausalStore[DotFun[V]]] = (left, right) => {
    CausalStore(
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
                                                       vSemiLattice: Lattice[CausalStore[V]]
                                                      ): Lattice[CausalStore[DotMap[K, V]]] =
    (left: CausalStore[DotMap[K, V]], right: CausalStore[DotMap[K, V]]) =>
      CausalStore(
        ((left.store.keySet union right.store.keySet) map { key =>
          val leftCausal  = CausalStore(left.store.getOrElse(key, DotStore[V].empty), left.context)
          val rightCausal = CausalStore(right.store.getOrElse(key, DotStore[V].empty), right.context)
          key -> Lattice[CausalStore[V]].merge(leftCausal, rightCausal).store
        } filterNot {
           case (key, dotStore) => DotStore[V].empty == dotStore
         }).toMap,
        left.context.union(right.context)
        )
}
