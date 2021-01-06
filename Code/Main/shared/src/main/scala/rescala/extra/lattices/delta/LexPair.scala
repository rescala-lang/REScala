package rescala.extra.lattices.delta

case class LexPair[A, B](fst: A, snd: B)

case object LexPair {
  implicit def LexPairAsUIJDLattice[A: UIJDLatticeWithBottom, B: UIJDLatticeWithBottom]: UIJDLatticeWithBottom[LexPair[A, B]] = new UIJDLatticeWithBottom[LexPair[A, B]] {
    override def bottom: LexPair[A, B] = LexPair(UIJDLatticeWithBottom[A].bottom, UIJDLatticeWithBottom[B].bottom)

    override def leq(left: LexPair[A, B], right: LexPair[A, B]): Boolean =
      UIJDLatticeWithBottom[A].leq(left.fst, right.fst) && (
        !UIJDLatticeWithBottom[A].leq(right.fst, left.fst) || UIJDLatticeWithBottom[B].leq(left.snd, right.snd)
      )

    /**
     * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
     */
    override def decompose(state: LexPair[A, B]): Set[LexPair[A, B]] =
      UIJDLatticeWithBottom[B].decompose(state.snd).map(LexPair(state.fst, _))

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: LexPair[A, B], right: LexPair[A, B]): LexPair[A, B] = {
      val lfleq = UIJDLatticeWithBottom[A].leq(left.fst, right.fst)
      val rfleq = UIJDLatticeWithBottom[A].leq(right.fst, left.fst)

      if (lfleq && !rfleq) right
      else if (rfleq && !lfleq) left
      else if (lfleq && rfleq) LexPair(left.fst, UIJDLatticeWithBottom[B].merge(left.snd, right.snd))
      else LexPair(UIJDLatticeWithBottom[A].merge(left.fst, right.fst), UIJDLatticeWithBottom[B].bottom)
    }
  }
}
