package kofre.decompose.interfaces

import kofre.base.{Bottom, DecomposeLattice, Id}
import kofre.base.Lattice.Operators
import kofre.decompose.*
import kofre.dotted.DottedDecompose
import kofre.syntax.OpsSyntaxHelper

/** A LexCounter is a Delta CRDT modeling a counter.
  *
  * It uses lexicographic pairs to allow counter decrements as well as increments.
  */
object LexCounterInterface {

  def empty: LexCounter = Map.empty

  /** A LexPair is a lexicographic pair of two values that is used with a lexicographical ordering in the state of
    * [[interfaces.LexCounterInterface]].
    */
  case class LexPair[A, B](fst: A, snd: B)

  case object LexPair {
    implicit def LexPairAsUIJDLattice[A: DecomposeLattice: Bottom, B: DecomposeLattice: Bottom]
        : DecomposeLattice[LexPair[A, B]] =
      new DecomposeLattice[LexPair[A, B]] {
        override def lteq(left: LexPair[A, B], right: LexPair[A, B]): Boolean =
          DecomposeLattice[A].lteq(left.fst, right.fst) && (
            !DecomposeLattice[A].lteq(right.fst, left.fst) || DecomposeLattice[B].lteq(left.snd, right.snd)
          )

        /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
        override def decompose(state: LexPair[A, B]): Iterable[LexPair[A, B]] =
          DecomposeLattice[B].decompose(state.snd).map(LexPair(state.fst, _))

        /** By assumption: associative, commutative, idempotent. */
        override def merge(left: LexPair[A, B], right: LexPair[A, B]): LexPair[A, B] = {
          val lfleq = left.fst <= right.fst
          val rfleq = right.fst <= left.fst

          if (lfleq && !rfleq) right
          else if (rfleq && !lfleq) left
          else if (lfleq && rfleq) LexPair(left.fst, left.snd merge right.snd)
          else LexPair(left.fst merge right.fst, Bottom[B].empty)
        }
      }
  }

  type LexCounter = Map[Id, LexPair[Int, Int]]

  given contextDecompose: DottedDecompose[LexCounter] = {
    given Bottom[Int] with { def empty: Int = Int.MinValue }
    given DecomposeLattice[Int] = DecomposeLattice.intMaxLattice
    DottedDecompose.liftDecomposeLattice
  }

  implicit class LexCounterSyntax[C](container: C) extends OpsSyntaxHelper[C, LexCounter](container) {

    def value(using QueryP): Int = current.values.map(_.snd).sum

    def inc()(using MutationIdP): C =
      current.get(replicaID) match {
        case None                => Map(replicaID -> LexPair(0, 1)).mutator
        case Some(LexPair(l, r)) => Map(replicaID -> LexPair(l, r + 1)).mutator
      }

    def dec()(using MutationIdP): C =
      current.get(replicaID) match {
        case None                => Map(replicaID -> LexPair(1, -1)).mutator
        case Some(LexPair(l, r)) => Map(replicaID -> LexPair(l + 1, r - 1)).mutator
      }
  }
}
