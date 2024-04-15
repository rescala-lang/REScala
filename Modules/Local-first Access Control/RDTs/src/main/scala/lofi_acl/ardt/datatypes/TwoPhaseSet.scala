package lofi_acl.ardt.datatypes

import lofi_acl.ardt.base.{Bottom, Lattice}

case class TwoPhaseSet[T](added: Set[T] = Set.empty[T], removed: Set[T] = Set.empty[T]):
  def contains(elem: T): Boolean =
    added.contains(elem) && !removed.contains(elem)

  def elements: Set[T] =
    added -- removed

object TwoPhaseSet {
  def empty[T]: TwoPhaseSet[T] = TwoPhaseSet(Set.empty, Set.empty)

  /* Another option would be to remove deleted elements from added
    case (TwoPhaseSet(leftAdded, leftRemoved), TwoPhaseSet(rightAdded, rightRemoved)) =>
      val newRemoved = leftRemoved ++ rightRemoved
      val newAdded   = (leftAdded ++ rightAdded) -- newRemoved
      TwoPhaseSet(newAdded, rightAdded)
   */
  import lofi_acl.ardt.base.StandardLibrary.GrowOnlySet.lattice
  given lattice[T]: Lattice[TwoPhaseSet[T]] = Lattice.derived

  given bottom[T]: Bottom[TwoPhaseSet[T]] with
    override val empty: TwoPhaseSet[T] = TwoPhaseSet.empty

  object mutators:
    def add[T](twoPhaseSet: TwoPhaseSet[T], element: T): TwoPhaseSet[T] =
      TwoPhaseSet(added = Set(element))

    def remove[T](twoPhaseSet: TwoPhaseSet[T], element: T): TwoPhaseSet[T] =
      TwoPhaseSet(removed = Set(element))

    def removeAll[T](elements: Set[T]): TwoPhaseSet[T] =
      TwoPhaseSet(removed = elements)

    def clear[T](twoPhaseSet: TwoPhaseSet[T]): TwoPhaseSet[T] =
      // twoPhaseSet.added -- twoPhaseSet.removed might result in smaller deltas.
      // Though it might make more sense to assume that the sets added and removed are disjoint. (see alt. merge)
      TwoPhaseSet(removed = twoPhaseSet.added)
}
