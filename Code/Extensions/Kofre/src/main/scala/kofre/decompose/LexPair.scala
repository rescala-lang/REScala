package kofre.decompose

/** A LexPair is a lexicographic pair of two values that is used with a lexicographical ordering in the state of
  * [[interfaces.LexCounterInterface]].
  */
case class LexPair[A, B](fst: A, snd: B)

case object LexPair {
  implicit def LexPairAsUIJDLattice[A: UIJDLattice, B: UIJDLattice]: UIJDLattice[LexPair[A, B]] =
    new UIJDLattice[LexPair[A, B]] {
      override def bottom: LexPair[A, B] = LexPair(UIJDLattice[A].bottom, UIJDLattice[B].bottom)

      override def leq(left: LexPair[A, B], right: LexPair[A, B]): Boolean =
        UIJDLattice[A].leq(left.fst, right.fst) && (
          !UIJDLattice[A].leq(right.fst, left.fst) || UIJDLattice[B].leq(left.snd, right.snd)
        )

      /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
      override def decompose(state: LexPair[A, B]): Iterable[LexPair[A, B]] =
        UIJDLattice[B].decompose(state.snd).map(LexPair(state.fst, _))

      /** By assumption: associative, commutative, idempotent. */
      override def merge(left: LexPair[A, B], right: LexPair[A, B]): LexPair[A, B] = {
        val lfleq = UIJDLattice[A].leq(left.fst, right.fst)
        val rfleq = UIJDLattice[A].leq(right.fst, left.fst)

        if (lfleq && !rfleq) right
        else if (rfleq && !lfleq) left
        else if (lfleq && rfleq) LexPair(left.fst, UIJDLattice[B].merge(left.snd, right.snd))
        else LexPair(UIJDLattice[A].merge(left.fst, right.fst), UIJDLattice[B].bottom)
      }
    }
}
