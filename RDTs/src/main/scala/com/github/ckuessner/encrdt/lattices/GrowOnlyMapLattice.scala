package com.github.ckuessner.encrdt.lattices

import com.github.ckuessner.encrdt.lattices.SemiLattice.merged

object GrowOnlyMapLattice {
  given gMapLattice[K, V: SemiLattice]: SemiLattice[Map[K, V]] = (left, right) =>
    right.foldLeft(left) { case (current, (key, r)) =>
      current.updatedWith(key) {
        case Some(l) => Some(merged(l, r))
        case None    => Some(r)
      }
    }
}
