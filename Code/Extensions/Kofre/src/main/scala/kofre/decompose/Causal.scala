package kofre.decompose

import kofre.causality.CContext

/** State type of causal CRDTs, which consist of a dot store and a causal context.
  *
  * @tparam D Type of the dot store
  * @tparam C Type of the causal context
  */
case class Causal[D, C](dotStore: D, cc: C)

case object Causal {
  implicit def CausalAsUIJDLattice[D: DotStore, C: CContext]: UIJDLattice[Causal[D, C]] =
    new UIJDLattice[Causal[D, C]] {
      override def leq(left: Causal[D, C], right: Causal[D, C]): Boolean = DotStore[D].leq(left, right)

      /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
      override def decompose(state: Causal[D, C]): Iterable[Causal[D, C]] = DotStore[D].decompose[C](state)

      /** By assumption: associative, commutative, idempotent. */
      override def merge(left: Causal[D, C], right: Causal[D, C]): Causal[D, C] = {
        val dsMerged = DotStore[D].mergePartial(left, right)
        val ccMerged = CContext[C].union(left.cc, right.cc)

        Causal(dsMerged, ccMerged)
      }

      override def bottom: Causal[D, C] = Causal(DotStore[D].empty, CContext[C].empty)
    }
}
