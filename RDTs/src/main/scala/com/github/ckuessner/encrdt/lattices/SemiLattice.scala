package com.github.ckuessner.encrdt.lattices

trait SemiLattice[T] {
  def merged(left: T, right: T): T
}

object SemiLattice {
  @inline def apply[A](implicit lattice: SemiLattice[A]): SemiLattice[A] = lattice

  def merged[A: SemiLattice](left: A, right: A): A = apply[A].merged(left, right)

  implicit def mapLattice[K, V: SemiLattice]: SemiLattice[Map[K, V]] = (left, right) =>
    right.foldLeft(left) {
      case (current, (key, r)) =>
        current.updatedWith(key) {
          case Some(l) => Some(merged(l,r))
          case None    => Some(r)
        }
    }

}
