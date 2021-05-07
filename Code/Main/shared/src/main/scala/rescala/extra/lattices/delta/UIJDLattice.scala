package rescala.extra.lattices.delta

import rescala.extra.lattices.Lattice
import rescala.extra.lattices.Lattice._

/** Extends the Lattice typeclass with the ability to compare states through unique irredundant join decomposition */
trait UIJDLattice[A] extends Lattice[A] {
  def leq(left: A, right: A): Boolean

  /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
  def decompose(state: A): Set[A]

  /** computes [[delta]] without [[state]] */
  def diff(state: A, delta: A): Option[A] = {
    decompose(delta).filter(!leq(_, state)).reduceOption(merge)
  }

  def bottom: A
}

object UIJDLattice {
  def apply[A](implicit l: UIJDLattice[A]): UIJDLattice[A] = l

  implicit def IntAsUIJDLattice: UIJDLattice[Int] = new UIJDLattice[Int] {
    override def leq(left: Int, right: Int): Boolean = left <= right
    override def decompose(state: Int): Set[Int]     = Set(state)
    override def merge(left: Int, right: Int): Int   = left max right
    override def bottom: Int                         = 0
  }

  implicit def SetAsUIJDLattice[A]: UIJDLattice[Set[A]] = new UIJDLattice[Set[A]] {
    override def leq(left: Set[A], right: Set[A]): Boolean  = left subsetOf right
    override def decompose(state: Set[A]): Set[Set[A]]      = state.map(Set(_))
    override def merge(left: Set[A], right: Set[A]): Set[A] = left union right
    override def bottom: Set[A]                             = Set.empty[A]
  }

  implicit def OptionAsUIJDLattice[A: UIJDLattice]: UIJDLattice[Option[A]] = new UIJDLattice[Option[A]] {
    override def leq(left: Option[A], right: Option[A]): Boolean = (left, right) match {
      case (None, _)          => true
      case (Some(_), None)    => false
      case (Some(l), Some(r)) => UIJDLattice[A].leq(l, r)
    }
    override def decompose(state: Option[A]): Set[Option[A]] = state match {
      case None    => Set(None)
      case Some(v) => UIJDLattice[A].decompose(v).map(Some(_))
    }
    override def merge(left: Option[A], right: Option[A]): Option[A] = optionLattice[A].merge(left, right)
    override def bottom: Option[A]                                   = Option.empty[A]
  }

  implicit def MapAsUIJDLattice[K, V: UIJDLattice]: UIJDLattice[Map[K, V]] = new UIJDLattice[Map[K, V]] {
    override def leq(left: Map[K, V], right: Map[K, V]): Boolean =
      left.keySet.forall { k =>
        OptionAsUIJDLattice[V].leq(left.get(k), right.get(k))
      }
    override def decompose(state: Map[K, V]): Set[Map[K, V]] = state.keySet.flatMap { k =>
      UIJDLattice[V].decompose(state(k)).map(v => Map(k -> v))
    }
    override def merge(left: Map[K, V], right: Map[K, V]): Map[K, V] = mapLattice[K, V].merge(left, right)
    override def bottom: Map[K, V]                                   = Map.empty[K, V]
  }

  implicit def PairAsUIJDLattice[A: UIJDLattice, B: UIJDLattice]: UIJDLattice[(A, B)] = new UIJDLattice[(A, B)] {
    override def bottom: (A, B) = (UIJDLattice[A].bottom, UIJDLattice[B].bottom)
    override def leq(left: (A, B), right: (A, B)): Boolean = (left, right) match {
      case ((ll, lr), (rl, rr)) =>
        UIJDLattice[A].leq(ll, rl) && UIJDLattice[B].leq(lr, rr)
    }
    override def decompose(state: (A, B)): Set[(A, B)] = state match {
      case (left, right) =>
        val leftDecomposed  = UIJDLattice[A].decompose(left) map { (_, UIJDLattice[B].bottom) }
        val rightDecomposed = UIJDLattice[B].decompose(right) map { (UIJDLattice[A].bottom, _) }
        leftDecomposed union rightDecomposed
    }
    override def merge(left: (A, B), right: (A, B)): (A, B) = (left, right) match {
      case ((ll, lr), (rl, rr)) =>
        (UIJDLattice[A].merge(ll, rl), UIJDLattice[B].merge(lr, rr))
    }
  }
}
