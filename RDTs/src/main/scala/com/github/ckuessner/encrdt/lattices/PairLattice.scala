package com.github.ckuessner.encrdt.lattices

object PairLattice {
  def pairLattice[A: SemiLattice, B: SemiLattice]: SemiLattice[(A, B)] =
    (left, right) => (
      SemiLattice[A].merged(left._1, right._1),
      SemiLattice[B].merged(left._2, right._2)
    )
}
