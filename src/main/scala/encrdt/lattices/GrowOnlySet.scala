package de.ckuessner
package encrdt.lattices

import encrdt.lattices.interfaces.{SemiLattice, SetCrdt}

class GrowOnlySet[T] extends SetCrdt[T] {
  private var _state: Set[T] = Set()

  def state: Set[T] = _state

  override def add(element: T): Unit = _state += element

  override def remove(element: T): Unit =
    throw new UnsupportedOperationException("Can't remove elements from GrowOnlySet")

  override def values: Set[T] = _state
}

object GrowOnlySetLattice {
  implicit def lattice[T]: SemiLattice[Set[T]] =
    (left: Set[T], right: Set[T]) => left ++ right
}