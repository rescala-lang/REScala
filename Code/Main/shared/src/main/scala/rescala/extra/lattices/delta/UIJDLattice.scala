package rescala.extra.lattices.delta

import rescala.extra.lattices.Lattice
import rescala.extra.lattices.Lattice._

/**
 * Extends the Lattice typeclass with the ability to compare states through unique irredundant join decomposition
 */
trait UIJDLattice[A] extends Lattice[A] {
  def leq(left: A, right: A): Boolean

  /**
   * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
   */
  def decompose(state: A): Set[A]

  def diff(state: A, delta: A): Option[A] = {
    decompose(delta).filter(!leq(_, state)).foldLeft(Option.empty[A]) {
      case (None, right) => Some(right)
      case (Some(left), right) => Some(merge(left, right))
    }
  }
}

object UIJDLattice {
  def apply[A](implicit l: UIJDLattice[A]): UIJDLattice[A] = l

  implicit def IntAsUIJDLattice: UIJDLattice[Int] = new UIJDLattice[Int] {
    override def leq(left: Int, right: Int): Boolean = left <= right

    /**
     * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
     */
    override def decompose(state: Int): Set[Int] = Set(state)

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: Int, right: Int): Int = left max right
  }
}

trait UIJDLatticeWithBottom[A] extends UIJDLattice[A] {
  def bottom: A
}

object UIJDLatticeWithBottom {
  def apply[A](implicit l: UIJDLatticeWithBottom[A]): UIJDLatticeWithBottom[A] = l

  implicit def IntAsUIJDLattice: UIJDLatticeWithBottom[Int] = new UIJDLatticeWithBottom[Int] {
    override def leq(left: Int, right: Int): Boolean = left <= right

    /**
      * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
      */
    override def decompose(state: Int): Set[Int] = Set(state)

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: Int, right: Int): Int = left max right

    override def bottom: Int = 0
  }

  implicit def SetAsUIJDLattice[A]: UIJDLatticeWithBottom[Set[A]] = new UIJDLatticeWithBottom[Set[A]] {
    override def leq(left: Set[A], right: Set[A]): Boolean = left subsetOf right

    /**
     * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
     */
    override def decompose(state: Set[A]): Set[Set[A]] = state.map(Set(_))

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: Set[A], right: Set[A]): Set[A] = left union right

    override def bottom: Set[A] = Set.empty[A]
  }

  implicit def OptionAsUIJDLattice[A: UIJDLattice]: UIJDLatticeWithBottom[Option[A]] = new UIJDLatticeWithBottom[Option[A]] {
    override def leq(left: Option[A], right: Option[A]): Boolean = (left, right) match {
      case (None, _) => true
      case (Some(_), None) => false
      case (Some(l), Some(r)) => UIJDLattice[A].leq(l, r)
    }

    /**
     * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
     */
    override def decompose(state: Option[A]): Set[Option[A]] = state match {
      case None => Set(None)
      case Some(v) => UIJDLattice[A].decompose(v).map(Some(_))
    }

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: Option[A], right: Option[A]): Option[A] = optionLattice[A].merge(left, right)

    override def bottom: Option[A] = Option.empty[A]
  }

  implicit def MapAsUIJDLattice[K, V: UIJDLattice]: UIJDLatticeWithBottom[Map[K, V]] = new UIJDLatticeWithBottom[Map[K, V]] {
    override def leq(left: Map[K, V], right: Map[K, V]): Boolean =
      left.keySet.forall { k =>
        OptionAsUIJDLattice[V].leq(left.get(k), right.get(k))
      }

    /**
     * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
     */
    override def decompose(state: Map[K, V]): Set[Map[K, V]] = state.keySet.flatMap { k =>
      UIJDLattice[V].decompose(state(k)).map(v => Map(k -> v))
    }

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: Map[K, V], right: Map[K, V]): Map[K, V] = mapLattice[K, V].merge(left, right)

    override def bottom: Map[K, V] = Map.empty[K, V]
  }

  implicit def PairAsUIJDLattice[A: UIJDLatticeWithBottom, B: UIJDLatticeWithBottom]: UIJDLatticeWithBottom[(A, B)] = new UIJDLatticeWithBottom[(A, B)] {
    override def bottom: (A, B) = (UIJDLatticeWithBottom[A].bottom, UIJDLatticeWithBottom[B].bottom)

    override def leq(left: (A, B), right: (A, B)): Boolean = (left, right) match {
      case ((ll, lr), (rl, rr)) =>
        UIJDLatticeWithBottom[A].leq(ll, rl) && UIJDLatticeWithBottom[B].leq(lr, rr)
    }

    /**
     * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
     */
    override def decompose(state: (A, B)): Set[(A, B)] = state match {
      case (left, right) =>
        val leftDecomposed = UIJDLatticeWithBottom[A].decompose(left) map { (_, UIJDLatticeWithBottom[B].bottom) }
        val rightDecomposed = UIJDLatticeWithBottom[B].decompose(right) map { (UIJDLatticeWithBottom[A].bottom, _) }
        leftDecomposed union rightDecomposed
    }

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: (A, B), right: (A, B)): (A, B) = (left, right) match {
      case ((ll, lr), (rl, rr)) =>
        (UIJDLatticeWithBottom[A].merge(ll, rl), UIJDLatticeWithBottom[B].merge(lr, rr))
    }
  }
}
