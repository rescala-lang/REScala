package de.ckuessner
package encrdt.lattices

case class GrowOnlySetLattice[T](values: Set[T]) {
  def added(element: T): GrowOnlySetLattice[T] = GrowOnlySetLattice(values + element)
}

object GrowOnlySetLattice {
  implicit def lattice[T]: SemiLattice[GrowOnlySetLattice[T]] = (l, r) => GrowOnlySetLattice(l.values ++ r.values)

  implicit def setLattice[T]: SemiLattice[Set[T]] =
    (left: Set[T], right: Set[T]) => left ++ right
}