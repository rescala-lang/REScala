package com.github.ckuessner.encrdt.lattices

case class GrowOnlySetLattice[T](values: Set[T]) {
  def added(element: T): GrowOnlySetLattice[T] = GrowOnlySetLattice(values + element)
}

object GrowOnlySetLattice {
  given lattice[T]: SemiLattice[GrowOnlySetLattice[T]] = (l, r) => GrowOnlySetLattice(l.values ++ r.values)

  given SetLattice[T]: SemiLattice[Set[T]] =
    (left: Set[T], right: Set[T]) => left ++ right
}
