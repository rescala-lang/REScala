package kofre.dotbased

import kofre.Lattice
import kofre.causality.{CausalContext, Dot}
import kofre.dotbased.DotStore
import kofre.dotbased.DotStore.{DotFun, DotMap, DotSet}
import kofre.Lattice.Operators
import kofre.decompose.UIJDLattice

case class CausalStore[A](store: A, context: CausalContext)

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
object CausalStore {
  def empty[D: DotStore]: CausalStore[D] = CausalStore(DotStore[D].empty, CausalContext.empty)

  // (s, c) ⨆ (s', c') = ((s ∩ s') ∪ (s \ c') ∪ (s' \ c), c ∪ c')
  implicit def CausalWithDotSetLattice: Lattice[CausalStore[Set[Dot]]] = UIJDLattice.CausalAsUIJDLattice

  // (s, c) ⨆ (s', c') = ((s ∩ s') ∪ (s \ c') ∪ (s' \ c), c ∪ c')
  implicit def CausalWithContextSetLattice: Lattice[CausalStore[CausalContext]] = UIJDLattice.CausalAsUIJDLattice

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
      left.context.merge(right.context)
    )
  }

  // (m, c) ⨆ (m', c') = ( {k -> v(k) | k ∈ dom m ∩ dom m' ∧ v(k) ≠ ⊥}, c ∪ c')
  //                      where v(k) = fst((m(k), c) ⨆ (m'(k), c'))
  implicit def CausalWithDotMapLattice[K, V: DotStore](implicit
      vLattice: Lattice[CausalStore[V]]
  ): Lattice[CausalStore[DotMap[K, V]]] =
    (left: CausalStore[DotMap[K, V]], right: CausalStore[DotMap[K, V]]) =>
      CausalStore(
        ((left.store.keySet union right.store.keySet) map { key =>
          val leftCausalStore  = CausalStore(left.store.getOrElse(key, DotStore[V].empty), left.context)
          val rightCausalStore = CausalStore(right.store.getOrElse(key, DotStore[V].empty), right.context)
          key -> Lattice.merge(leftCausalStore, rightCausalStore).store
        } filterNot {
          case (key, store) => DotStore[V].empty == store
        }).toMap,
        left.context.union(right.context)
      )
}
