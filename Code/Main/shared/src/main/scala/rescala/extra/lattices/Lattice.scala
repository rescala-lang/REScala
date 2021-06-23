package rescala.extra.lattices

/** Well, its technically a semilattice, but that is just more to type. */
trait Lattice[A] {

  /** By assumption: associative, commutative, idempotent.
    *
    * For use with Delta CRDTs, this function should be optimized for the case that left >> right.
    */
  def merge(left: A, right: A): A
}

object Lattice {
  def apply[A](implicit ev: Lattice[A]): Lattice[A] = ev
  def merge[A: Lattice](left: A, right: A)          = apply[A].merge(left, right)

  implicit class LatticeOps[A](val lattice: A) {
    def merge(other: A)(implicit ev: Lattice[A]): A = ev.merge(lattice, other)
  }

  implicit def setInstance[A]: Lattice[Set[A]] =
    new Lattice[Set[A]] {
      override def merge(left: Set[A], right: Set[A]): Set[A] = left.union(right)
    }

  implicit def optionLattice[A: Lattice]: Lattice[Option[A]] =
    new Lattice[Option[A]] {
      override def merge(left: Option[A], right: Option[A]): Option[A] =
        (left, right) match {
          case (None, r)          => r
          case (l, None)          => l
          case (Some(l), Some(r)) => Some(Lattice.merge[A](l, r))
        }
    }

  implicit def mapLattice[K, V: Lattice]: Lattice[Map[K, V]] =
    new Lattice[Map[K, V]] {
      override def merge(left: Map[K, V], right: Map[K, V]): Map[K, V] =
        (left.keysIterator ++ right.keysIterator)
          .toSet[K].iterator
          .flatMap { key =>
            Lattice.merge(left.get(key), right.get(key)).map(key -> _)
          }.toMap
    }
}
