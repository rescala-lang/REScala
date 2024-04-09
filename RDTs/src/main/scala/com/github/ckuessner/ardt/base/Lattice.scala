package com.github.ckuessner.ardt.base

trait Lattice[T] {
  def merge(left: T, right: T): T
}

object Lattice {
  inline def apply[A](using lattice: Lattice[A]): Lattice[A] = lattice

  def merge[A: Lattice](left: A, right: A): A = apply[A].merge(left, right)
}
