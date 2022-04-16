package kofre.decompose.interfaces

import kofre.Lattice.Operators
import kofre.decompose.*
import kofre.syntax.{DeltaMutator, DeltaQuery, OpsSyntaxHelper}
import kofre.decompose.UIJDLattice.Operators

/** A LexCounter is a Delta CRDT modeling a counter.
  *
  * It uses lexicographic pairs to allow counter decrements as well as increments.
  */
object LexCounterInterface {

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
          val lfleq = left.fst <= right.fst
          val rfleq = right.fst <= left.fst

          if (lfleq && !rfleq) right
          else if (rfleq && !lfleq) left
          else if (lfleq && rfleq) LexPair(left.fst, left.snd merge right.snd)
          else LexPair(left.fst merge right.fst, UIJDLattice[B].bottom)
        }
      }
  }

  type LexCounter = Map[String, LexPair[Int, Int]]

  implicit class LexCounterSyntax[C, E](container: C) extends OpsSyntaxHelper[C, LexCounter](container) {

    def value(using QueryP): Int = current.values.map(_.snd).sum

    def inc()(using MutationIDP): C =
      current.get(replicaID) match {
        case None                => Map(replicaID -> LexPair(0, 1))
        case Some(LexPair(l, r)) => Map(replicaID -> LexPair(l, r + 1))
      }

    def dec()(using MutationIDP): C =
      current.get(replicaID) match {
        case None                => Map(replicaID -> LexPair(1, -1))
        case Some(LexPair(l, r)) => Map(replicaID -> LexPair(l + 1, r - 1))
      }
  }
}
