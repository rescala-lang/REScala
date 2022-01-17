package kofre.encrdt.lattices
import kofre.Lattice

object OptionLattice {
  implicit def optLattice[T: Lattice]: Lattice[Option[T]] = (l: Option[T], r: Option[T]) =>
    (l, r) match {
      case (None, None)             => None
      case (Some(lVal), Some(rVal)) => Some(Lattice.merge(lVal, rVal))
      case (Some(lVal), None)       => Some(lVal)
      case (None, Some(rVal))       => Some(rVal)
    }
}
