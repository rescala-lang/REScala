package lofi_acl.ardt.base

import rdts.base.{Bottom, Lattice}
import rdts.time.{Dot, Dots}

case class Causal[D](data: D, context: Dots)

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
object Causal {
  def bottom[D: Bottom]: Causal[D] = Causal(Bottom[D].empty, Dots.empty)

  // (s, c) ⨆ (s', c') = ((s ∩ s') ∪ (s \ c') ∪ (s' \ c), c ∪ c')
  given CausalWithDotSetLattice: Lattice[Causal[Dots]] = (left, right) => {
    val inBoth     = left.data `intersect` right.data
    val newInLeft  = left.data `subtract` right.context
    val newInRight = right.data `subtract` left.context

    val mergedCausalContext = left.context.union(right.context)
    Causal(inBoth `union` newInLeft `union` newInRight, mergedCausalContext)
  }

  // (m, c) ⨆ (m', c') = ( {k -> m(k) ⨆ m'(k) | k ∈ dom m ∩ dom m'} ∪
  //                       {(d, v) ∈ m  | d ∉ c'} ∪
  //                       {(d, v) ∈ m' | d ∉ c},
  //                      c ∪ c')
  given CausalWithDotFunLattice[V: Lattice]: Lattice[Causal[Map[Dot, V]]] = (left, right) => {
    Causal(
      ((left.data.keySet `intersect` right.data.keySet) map { (dot: Dot) =>
        (dot, Lattice.merge(left.data(dot), right.data(dot)))
      }).toMap
      ++ left.data.filterNot { case (dot, _) => right.context.contains(dot) }
      ++ right.data.filterNot { case (dot, _) => left.context.contains(dot) },
      left.context.union(right.context)
    )
  }

  // (m, c) ⨆ (m', c') = ( {k -> v(k) | k ∈ dom m ∩ dom m' ∧ v(k) ≠ ⊥}, c ∪ c')
  //                      where v(k) = fst((m(k), c) ⨆ (m'(k), c'))
  given CausalWithDotMapLattice[K, V: Bottom](using Lattice[Causal[V]]): Lattice[Causal[Map[K, V]]] =
    (left: Causal[Map[K, V]], right: Causal[Map[K, V]]) =>
      Causal(
        ((left.data.keySet union right.data.keySet) map { key =>
          val leftCausal  = Causal(left.data.getOrElse(key, Bottom[V].empty), left.context)
          val rightCausal = Causal(right.data.getOrElse(key, Bottom[V].empty), right.context)
          key -> Lattice[Causal[V]].merge(leftCausal, rightCausal).data
        } filterNot { case (key, dotStore) =>
          Bottom[V].empty == dotStore
        }).toMap,
        left.context.union(right.context)
      )
}
