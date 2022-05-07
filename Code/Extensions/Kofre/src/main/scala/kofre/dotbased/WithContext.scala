package kofre.dotbased

import kofre.Lattice
import kofre.causality.{CausalContext, Dot}
import kofre.dotbased.AsCausalContext
import kofre.dotbased.AsCausalContext.DotFun
import kofre.Lattice.Operators
import kofre.decompose.{DecomposableDotStore, UIJDLattice}

case class WithContext[A](store: A, context: CausalContext)


trait WithContextMerge[A] {
  def mergePartial(left: WithContext[A], right: WithContext[A]): A
}

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
object WithContext {
  def empty[D: AsCausalContext]: WithContext[D] = WithContext(AsCausalContext[D].empty, CausalContext.empty)


  // (s, c) ⨆ (s', c') = ((s ∩ s') ∪ (s \ c') ∪ (s' \ c), c ∪ c')
  implicit def CausalWithDotSetLattice: Lattice[WithContext[Set[Dot]]] = {
    given DecomposableDotStore[Set[Dot]] = DecomposableDotStore.UIJDLatticeAsDecomposableDotStore
    UIJDLattice.contextUIJDLattice
  }

  // (s, c) ⨆ (s', c') = ((s ∩ s') ∪ (s \ c') ∪ (s' \ c), c ∪ c')
  implicit def CausalWithContextSetLattice: Lattice[WithContext[CausalContext]] = UIJDLattice.contextUIJDLattice


  given withContextFun[A: Lattice]: WithContextMerge[Map[Dot, A]] with {
    override def mergePartial(left: WithContext[Map[Dot, A]], right: WithContext[Map[Dot, A]]): Map[Dot, A] = {
      val fromLeft = left.store.filter { case (dot, _) => !right.context.contains(dot) }

      right.store.foldLeft(fromLeft) {
        case (m, (dot, r)) =>
          left.store.get(dot) match {
            case None =>
              if (left.context.contains(dot)) m
              else m.updated(dot, r)
            case Some(l) => m.updated(dot, Lattice[A].merge(l, r))
          }
      }
    }
  }

  // (m, c) ⨆ (m', c') = ( {k -> m(k) ⨆ m'(k) | k ∈ dom m ∩ dom m'} ∪
  //                       {(d, v) ∈ m  | d ∉ c'} ∪
  //                       {(d, v) ∈ m' | d ∉ c},
  //                      c ∪ c')
  implicit def CausalWithDotFunLattice[V: Lattice]: Lattice[WithContext[DotFun[V]]] = (left, right) => {
    WithContext(
      withContextFun.mergePartial(left, right),
      left.context.merge(right.context)
    )
  }
  

  // (m, c) ⨆ (m', c') = ( {k -> v(k) | k ∈ dom m ∩ dom m' ∧ v(k) ≠ ⊥}, c ∪ c')
  //                      where v(k) = fst((m(k), c) ⨆ (m'(k), c'))
  implicit def CausalWithDotMapLattice[K, V: AsCausalContext](implicit vlattice: Lattice[WithContext[V]]): Lattice[WithContext[Map[K, V]]] =
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
