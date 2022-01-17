
package encrdt.lattices
import kofre.Lattice
case class TwoPhaseSetLattice[T](addedElems: Set[T] = Set[T](), removedElems: Set[T] = Set[T]()) {
  def values: Set[T] = addedElems -- removedElems

  def added(element: T): TwoPhaseSetLattice[T] = copy(addedElems = addedElems + element)

  def removed(element: T): TwoPhaseSetLattice[T] = {
    copy(removedElems = removedElems + element)
  }

  def contains(element: T): Boolean = addedElems.contains(element) && !removedElems.contains(element)
}

object TwoPhaseSetLattice {
  implicit def TwoPhaseSetSemiLattice[T]: Lattice[TwoPhaseSetLattice[T]] =
    (left: TwoPhaseSetLattice[T], right: TwoPhaseSetLattice[T]) =>
      TwoPhaseSetLattice(left.addedElems ++ right.addedElems, left.removedElems ++ right.removedElems)
}
