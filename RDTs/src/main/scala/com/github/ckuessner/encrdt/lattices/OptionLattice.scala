package com.github.ckuessner.encrdt.lattices

object OptionLattice {
  implicit def optLattice[T: SemiLattice]: SemiLattice[Option[T]] = (l: Option[T], r: Option[T]) =>
    (l, r) match {
      case (None, None)             => None
      case (Some(lVal), Some(rVal)) => Some(SemiLattice.merged(lVal, rVal))
      case (Some(lVal), None)       => Some(lVal)
      case (None, Some(rVal))       => Some(rVal)
    }
}
