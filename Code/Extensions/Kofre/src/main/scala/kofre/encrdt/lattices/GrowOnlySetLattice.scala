
package encrdt.lattices
import kofre.Lattice
case class GrowOnlySetLattice[T](values: Set[T]) {
  def added(element: T): GrowOnlySetLattice[T] = GrowOnlySetLattice(values + element)
}

object GrowOnlySetLattice {
  implicit def lattice[T]: Lattice[GrowOnlySetLattice[T]] = (l, r) => GrowOnlySetLattice(l.values ++ r.values)

  implicit def SetLattice[T]: Lattice[Set[T]] =
    (left: Set[T], right: Set[T]) => left ++ right
}
