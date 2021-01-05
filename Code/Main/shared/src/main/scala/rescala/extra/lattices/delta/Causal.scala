package rescala.extra.lattices.delta

case class Causal[D: DotStore, C: CContext](dotStore: D, cc: C)

case object Causal {
  implicit def CausalAsUIJDLattice[D: DotStore, C: CContext]: UIJDLatticeWithBottom[Causal[D, C]] = new UIJDLatticeWithBottom[Causal[D, C]] {
    override def leq(left: Causal[D, C], right: Causal[D, C]): Boolean =
      DotStore[D].leq[C, C](left.dotStore, left.cc, right.dotStore, right.cc)

    /**
     * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
     */
    override def decompose(state: Causal[D, C]): Set[Causal[D, C]] =
      DotStore[D].decompose[C](state.dotStore, state.cc) map {
        case (dotStore, cc) => Causal(dotStore, cc)
      }

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: Causal[D, C], right: Causal[D, C]): Causal[D, C] =
      DotStore[D].merge(left.dotStore, left.cc, right.dotStore, right.cc) match {
        case (dotStore, cc) => Causal(dotStore, cc)
      }

    override def bottom: Causal[D, C] = Causal(DotStore[D].empty, CContext[C].empty)
  }
}
