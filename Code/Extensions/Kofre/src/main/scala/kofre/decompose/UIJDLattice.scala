package kofre.decompose

import kofre.Lattice
import kofre.Lattice.optionLattice
import kofre.causality.{CausalStore, CausalContext}
import kofre.Lattice.Operators

/** Extends the Lattice typeclass with the ability to compare states through unique irredundant join decomposition */
trait UIJDLattice[A] extends Lattice[A] {
  def leq(left: A, right: A): Boolean

  /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
  def decompose(state: A): Iterable[A]

  /** computes delta without state */
  def diff(state: A, delta: A): Option[A] = {
    decompose(delta).filter(!leq(_, state)).reduceOption(merge)
  }

  def bottom: A
}

/** reuse existing lattice instance to implement a UIJDLattice */
trait UIJDFromLattice[A](using lattice: Lattice[A]) extends UIJDLattice[A] {
  export lattice.merge
}

object UIJDLattice {
  def apply[A](implicit l: UIJDLattice[A]): UIJDLattice[A] = l

  // this seems to have better 2.13 compatibility
  implicit class Operators[A: UIJDLattice](left: A):
    def leq(right: A): Boolean = UIJDLattice[A].leq(left, right)
    def decompose: Iterable[A] = UIJDLattice[A].decompose(left)

  implicit def IntAsUIJDLattice: UIJDLattice[Int] = new UIJDFromLattice[Int](using _ max _) {
    override def leq(left: Int, right: Int): Boolean  = left <= right
    override def decompose(state: Int): Iterable[Int] = List(state)
    override def bottom: Int                          = 0
  }

  implicit def SetAsUIJDLattice[A]: UIJDLattice[Set[A]] = new UIJDFromLattice[Set[A]] {
    override def leq(left: Set[A], right: Set[A]): Boolean  = left subsetOf right
    override def decompose(state: Set[A]): Iterable[Set[A]] = state.map(Set(_))
    override def bottom: Set[A]                             = Set.empty[A]
  }

  implicit def OptionAsUIJDLattice[A: UIJDLattice]: UIJDLattice[Option[A]] = new UIJDFromLattice[Option[A]] {
    override def leq(left: Option[A], right: Option[A]): Boolean = (left, right) match {
      case (None, _)          => true
      case (Some(_), None)    => false
      case (Some(l), Some(r)) => l leq r
    }

    override def decompose(state: Option[A]): Iterable[Option[A]] = state match {
      case None    => List.empty[Option[A]]
      case Some(v) => v.decompose.map(Some(_))
    }

    override def bottom: Option[A] = Option.empty[A]
  }

  implicit def MapAsUIJDLattice[K, V: UIJDLattice]: UIJDLattice[Map[K, V]] = new UIJDFromLattice[Map[K, V]] {
    override def leq(left: Map[K, V], right: Map[K, V]): Boolean =
      left.keySet.forall { k =>
        left.get(k) leq right.get(k)
      }

    override def decompose(state: Map[K, V]): Iterable[Map[K, V]] = state.keys.flatMap { k =>
      UIJDLattice[V].decompose(state(k)).map(v => Map(k -> v)) match {
        case s if s.isEmpty => List(Map(k -> state(k)))
        case s              => s
      }
    }

    override def bottom: Map[K, V] = Map.empty[K, V]
  }

  implicit def PairAsUIJDLattice[A: UIJDLattice, B: UIJDLattice]: UIJDLattice[(A, B)] = new UIJDFromLattice[(A, B)] {
    override def bottom: (A, B) = (UIJDLattice[A].bottom, UIJDLattice[B].bottom)

    override def leq(left: (A, B), right: (A, B)): Boolean = (left, right) match {
      case ((ll, lr), (rl, rr)) =>
        (ll leq rl) && (lr leq rr)
    }

    override def decompose(state: (A, B)): Iterable[(A, B)] = state match {
      case (left, right) =>
        val leftDecomposed  = left.decompose.map { (_, UIJDLattice[B].bottom) }
        val rightDecomposed = right.decompose.map { (UIJDLattice[A].bottom, _) }
        leftDecomposed ++ rightDecomposed
    }
  }

  implicit def TripleAsUIJDLattice[A: UIJDLattice, B: UIJDLattice, C: UIJDLattice]: UIJDLattice[(A, B, C)] =
    new UIJDFromLattice[(A, B, C)] {
      override def leq(left: (A, B, C), right: (A, B, C)): Boolean = (left, right) match {
        case ((la, lb, lc), (ra, rb, rc)) =>
          (la leq ra) && (lb leq rb) && (lc leq rc)
      }

      override def decompose(state: (A, B, C)): Iterable[(A, B, C)] = state match {
        case (a, b, c) =>
          val aDecomposed = a.decompose.map { (_, UIJDLattice[B].bottom, UIJDLattice[C].bottom) }
          val bDecomposed = b.decompose.map { (UIJDLattice[A].bottom, _, UIJDLattice[C].bottom) }
          val cDecomposed = c.decompose.map { (UIJDLattice[A].bottom, UIJDLattice[B].bottom, _) }
          aDecomposed ++ bDecomposed ++ cDecomposed
      }

      override def bottom: (A, B, C) = (UIJDLattice[A].bottom, UIJDLattice[B].bottom, UIJDLattice[C].bottom)
    }

  def AtomicUIJDLattice[A]: UIJDLattice[A] = new UIJDLattice[A] {
    override def leq(left: A, right: A): Boolean  = false
    override def decompose(state: A): Iterable[A] = List(state)
    override def bottom: A = throw new UnsupportedOperationException("Can't compute bottom of atomic type A")
    override def merge(left: A, right: A): A =
      if left == right then left
      else throw new UnsupportedOperationException(s"Can't merge atomic type A, left: $left, right: $right")
  }

  given causalLattice[D: DotStore]: Lattice[CausalStore[D]] = (left, right) =>
    val dsMerged = DotStore[D].mergePartial(left, right)
    val ccMerged = left.context merge right.context
    CausalStore[D](dsMerged, ccMerged)

  implicit def CausalAsUIJDLattice[D: DotStore]: UIJDLattice[CausalStore[D]] = new UIJDFromLattice[CausalStore[D]] {
    override def leq(left: CausalStore[D], right: CausalStore[D]): Boolean = DotStore[D].leq(left, right)

    /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: CausalStore[D]): Iterable[CausalStore[D]] = DotStore[D].decompose(state)

    override def bottom: CausalStore[D] = CausalStore(DotStore[D].empty, CausalContext.empty)
  }

}
