package kofre.decompose

import kofre.Lattice
import kofre.Lattice.{Operators, mapLattice, optionLattice, setLattice}
import kofre.causality.CausalContext
import kofre.dotbased.WithContext

trait Bottom[A] { def empty: A }
trait Decompose[A] {
  /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
  def decompose(a: A): Iterable[A]
}

/** Extends the Lattice typeclass with the ability to compare states through unique irredundant join decomposition */
trait UIJDLattice[A] extends Lattice[A], Bottom[A], Decompose[A] {

  /** computes delta without state */
  def diff(state: A, delta: A): Option[A] = {
    decompose(delta).filter(!leq(_, state)).reduceOption(merge)
  }

}

/** reuse existing lattice instance to implement a UIJDLattice */
trait UIJDFromLattice[A](lattice: Lattice[A]) extends UIJDLattice[A] {
  export lattice.merge
}

object UIJDLattice {
  def apply[A](implicit l: UIJDLattice[A]): UIJDLattice[A] = l
  def bottom[A](implicit l: UIJDLattice[A]): A             = l.empty

  implicit class Operators[A: UIJDLattice](left: A):
    def <=(right: A): Boolean  = UIJDLattice[A].leq(left, right)
    def decompose: Iterable[A] = UIJDLattice[A].decompose(left)

  given IntAsUIJDLattice: UIJDLattice[Int] = new UIJDFromLattice[Int](_ max _) {
    override def leq(left: Int, right: Int): Boolean  = left <= right
    override def decompose(state: Int): Iterable[Int] = List(state)
    override def empty: Int                          = 0
  }

  given SetAsUIJDLattice[A]: UIJDLattice[Set[A]] = new UIJDFromLattice[Set[A]](setLattice) {
    override def leq(left: Set[A], right: Set[A]): Boolean  = left subsetOf right
    override def decompose(state: Set[A]): Iterable[Set[A]] = state.map(Set(_))
    override def empty: Set[A]                             = Set.empty[A]
  }

  given OptionAsUIJDLattice[A: UIJDLattice]: UIJDLattice[Option[A]] =
    new UIJDFromLattice[Option[A]](optionLattice) {
      override def leq(left: Option[A], right: Option[A]): Boolean = (left, right) match {
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

  given MapAsUIJDLattice[K, V: UIJDLattice]: UIJDLattice[Map[K, V]] =
    new UIJDFromLattice[Map[K, V]](mapLattice) {
      override def leq(left: Map[K, V], right: Map[K, V]): Boolean =
        left.keySet.forall { k =>
          left.get(k) <= right.get(k)
        }

      override def decompose(state: Map[K, V]): Iterable[Map[K, V]] = state.keys.flatMap { k =>
        UIJDLattice[V].decompose(state(k)).map(v => Map(k -> v)) match {
          case s if s.isEmpty => List(Map(k -> state(k)))
          case s              => s
        }
      }

      override def empty: Map[K, V] = Map.empty[K, V]
    }

  given PairAsUIJDLattice[A: UIJDLattice, B: UIJDLattice]: UIJDLattice[(A, B)] =
    new UIJDFromLattice[(A, B)](Lattice.derived) {
      override def empty: (A, B) = (UIJDLattice[A].empty, UIJDLattice[B].empty)

      override def leq(left: (A, B), right: (A, B)): Boolean = (left, right) match {
        case ((ll, lr), (rl, rr)) =>
          (ll <= rl) && (lr <= rr)
      }

      override def decompose(state: (A, B)): Iterable[(A, B)] = state match {
        case (left, right) =>
          val leftDecomposed  = left.decompose.map { (_, UIJDLattice[B].empty) }
          val rightDecomposed = right.decompose.map { (UIJDLattice[A].empty, _) }
          leftDecomposed ++ rightDecomposed
      }
    }

  given TripleAsUIJDLattice[A: UIJDLattice, B: UIJDLattice, C: UIJDLattice]: UIJDLattice[(A, B, C)] =
    new UIJDFromLattice[(A, B, C)](Lattice.derived) {
      override def leq(left: (A, B, C), right: (A, B, C)): Boolean = (left, right) match {
        case ((la, lb, lc), (ra, rb, rc)) =>
          (la <= ra) && (lb <= rb) && (lc <= rc)
      }

      override def decompose(state: (A, B, C)): Iterable[(A, B, C)] = state match {
        case (a, b, c) =>
          val aDecomposed = a.decompose.map { (_, UIJDLattice[B].empty, UIJDLattice[C].empty) }
          val bDecomposed = b.decompose.map { (UIJDLattice[A].empty, _, UIJDLattice[C].empty) }
          val cDecomposed = c.decompose.map { (UIJDLattice[A].empty, UIJDLattice[B].empty, _) }
          aDecomposed ++ bDecomposed ++ cDecomposed
      }

      override def empty: (A, B, C) = (UIJDLattice[A].empty, UIJDLattice[B].empty, UIJDLattice[C].empty)
    }

  def AtomicUIJDLattice[A]: UIJDLattice[A] = new UIJDLattice[A] {
    override def leq(left: A, right: A): Boolean  = false
    override def decompose(state: A): Iterable[A] = List(state)
    override def empty: A = throw new UnsupportedOperationException("Can't compute bottom of atomic type A")
    override def merge(left: A, right: A): A =
      if left == right then left
      else throw new UnsupportedOperationException(s"Can't merge atomic type A, left: $left, right: $right")
  }

  given contextLattice[D: DecomposableDotStore]: Lattice[WithContext[D]] = (left, right) =>
    val dsMerged = DecomposableDotStore[D].mergePartial(left, right)
    val ccMerged = left.context merge right.context
    WithContext[D](dsMerged, ccMerged)

  given contextUIJDLattice[D: DecomposableDotStore]: UIJDLattice[WithContext[D]] =
    new UIJDFromLattice[WithContext[D]](contextLattice) {
      override def leq(left: WithContext[D], right: WithContext[D]): Boolean = DecomposableDotStore[D].leq(left, right)

      /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
      override def decompose(state: WithContext[D]): Iterable[WithContext[D]] = DecomposableDotStore[D].decompose(state)

      override def empty: WithContext[D] = WithContext(DecomposableDotStore[D].empty, CausalContext.empty)
    }

}
