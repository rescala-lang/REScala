package kofre.decompose

import kofre.base.Lattice
import kofre.base.Lattice.{Operators, mapLattice, optionLattice, setLattice}
import kofre.base.Bottom
import kofre.causality.CausalContext
import kofre.contextual.{WithContext, WithContextDecompose}

trait Decompose[A] {
  /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
  def decompose(a: A): Iterable[A]
}

/** Extends the Lattice typeclass with the ability to compare states through unique irredundant join decomposition */
trait DecomposeLattice[A] extends Lattice[A], Bottom[A], Decompose[A] {

  /** computes delta without state */
  def diff(state: A, delta: A): Option[A] = {
    decompose(delta).filter(!lteq(_, state)).reduceOption(merge)
  }

}

/** reuse existing lattice instance to implement a DecomposeLattice */
trait DecomposeFromLattice[A](lattice: Lattice[A]) extends DecomposeLattice[A] {
  export lattice.merge
}

object DecomposeLattice {
  def apply[A](implicit l: DecomposeLattice[A]): DecomposeLattice[A] = l
  def bottom[A](implicit l: DecomposeLattice[A]): A             = l.empty

  implicit class Operators[A: DecomposeLattice](left: A):
    @scala.annotation.targetName("lteq")
    def <=(right: A): Boolean  = DecomposeLattice[A].lteq(left, right)
    def decompose: Iterable[A] = DecomposeLattice[A].decompose(left)

  val IntAsUIJDLattice: DecomposeLattice[Int] = new DecomposeFromLattice[Int](_ max _) {
    override def lteq(left: Int, right: Int): Boolean  = left <= right
    override def decompose(state: Int): Iterable[Int] = List(state)
    override def empty: Int                          = 0
  }

  given SetAsUIJDLattice[A]: DecomposeLattice[Set[A]] = new DecomposeFromLattice[Set[A]](setLattice) {
    override def lteq(left: Set[A], right: Set[A]): Boolean  = left subsetOf right
    override def decompose(state: Set[A]): Iterable[Set[A]] = state.map(Set(_))
    override def empty: Set[A]                             = Set.empty[A]
  }

  given OptionAsUIJDLattice[A: DecomposeLattice]: DecomposeLattice[Option[A]] =
    new DecomposeFromLattice[Option[A]](optionLattice) {
      override def lteq(left: Option[A], right: Option[A]): Boolean = (left, right) match {
        case (None, _)          => true
        case (Some(_), None)    => false
        case (Some(l), Some(r)) => l <= r
      }

      override def decompose(state: Option[A]): Iterable[Option[A]] = state match {
        case None    => List.empty[Option[A]]
        case Some(v) => v.decompose.map(Some(_))
      }

      override def empty: Option[A] = Option.empty[A]
    }

  given MapAsUIJDLattice[K, V: DecomposeLattice]: DecomposeLattice[Map[K, V]] =
    new DecomposeFromLattice[Map[K, V]](mapLattice) {
      override def lteq(left: Map[K, V], right: Map[K, V]): Boolean =
        left.keySet.forall { k =>
          left.get(k) <= right.get(k)
        }

      override def decompose(state: Map[K, V]): Iterable[Map[K, V]] = state.keys.flatMap { k =>
        DecomposeLattice[V].decompose(state(k)).map(v => Map(k -> v)) match {
          case s if s.isEmpty => List(Map(k -> state(k)))
          case s              => s
        }
      }

      override def empty: Map[K, V] = Map.empty[K, V]
    }

  given PairAsUIJDLattice[A: DecomposeLattice, B: DecomposeLattice]: DecomposeLattice[(A, B)] =
    new DecomposeFromLattice[(A, B)](Lattice.derived) {
      override def empty: (A, B) = (DecomposeLattice[A].empty, DecomposeLattice[B].empty)

      override def lteq(left: (A, B), right: (A, B)): Boolean = (left, right) match {
        case ((ll, lr), (rl, rr)) =>
          (ll <= rl) && (lr <= rr)
      }

      override def decompose(state: (A, B)): Iterable[(A, B)] = state match {
        case (left, right) =>
          val leftDecomposed  = left.decompose.map { (_, DecomposeLattice[B].empty) }
          val rightDecomposed = right.decompose.map { (DecomposeLattice[A].empty, _) }
          leftDecomposed ++ rightDecomposed
      }
    }

  given TripleAsUIJDLattice[A: DecomposeLattice, B: DecomposeLattice, C: DecomposeLattice]: DecomposeLattice[(A, B, C)] =
    new DecomposeFromLattice[(A, B, C)](Lattice.derived) {
      override def lteq(left: (A, B, C), right: (A, B, C)): Boolean = (left, right) match {
        case ((la, lb, lc), (ra, rb, rc)) =>
          (la <= ra) && (lb <= rb) && (lc <= rc)
      }

      override def decompose(state: (A, B, C)): Iterable[(A, B, C)] = state match {
        case (a, b, c) =>
          val aDecomposed = a.decompose.map { (_, DecomposeLattice[B].empty, DecomposeLattice[C].empty) }
          val bDecomposed = b.decompose.map { (DecomposeLattice[A].empty, _, DecomposeLattice[C].empty) }
          val cDecomposed = c.decompose.map { (DecomposeLattice[A].empty, DecomposeLattice[B].empty, _) }
          aDecomposed ++ bDecomposed ++ cDecomposed
      }

      override def empty: (A, B, C) = (DecomposeLattice[A].empty, DecomposeLattice[B].empty, DecomposeLattice[C].empty)
    }

  given contextUIJDLattice[D](using wcd: WithContextDecompose[D]): DecomposeLattice[WithContext[D]] =
    new DecomposeFromLattice[WithContext[D]](Lattice.contextLattice) {
      export wcd.decompose
      // needs manual override as export can not override :(
      override def lteq(left: WithContext[D], right: WithContext[D]): Boolean = wcd.lteq(left, right)
      override def empty: WithContext[D] = WithContext(wcd.empty, CausalContext.empty)
    }

}
