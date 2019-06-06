package rescala.lattices

/** Well, its technically a semilattice, but that is just more to type. */
trait Lattice[A] {
  /** Associative, commutative, idempotent. **/
  def merge(left: A, right: A): A
}

object Lattice {
  def apply[A](implicit ev: Lattice[A]): Lattice[A] = ev
  def merge[A: Lattice](left: A, right: A) = apply[A].merge(left, right)

  implicit class LatticeOps[A](val lattice: A)  {
    def merge(other: A)(implicit ev: Lattice[A]): A = ev.merge(lattice, other)
  }

  implicit def setInstance[A]: Lattice[Set[A]]  =
    new Lattice[Set[A]] {
      override def merge(left: Set[A], right: Set[A]): Set[A] = left.union(right)
    }

}
