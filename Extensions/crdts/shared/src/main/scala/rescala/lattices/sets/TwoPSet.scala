package rescala.lattices.sets

import rescala.lattices.Lattice

/**
  * Two phase set where elements can be added and removed but never added again.
  *
  * @param payload The payload consisting of one set for added entries and one set for removed entries (tombstones).
  * @tparam A The type of the elements in the set.
  */
case class TwoPSet[A](entries: Set[A], tombstones: Set[A]) extends RemovableCRDTSet[A] {

  override def add(e: A): TwoPSet[A] = TwoPSet(entries + e, tombstones)

  override def remove(e: A): TwoPSet[A] = if (entries(e)) TwoPSet(entries, tombstones + e) else this
  def remove(e: Seq[A]): TwoPSet[A] = TwoPSet(entries, tombstones ++ entries.intersect(e.toSet))

  override def contains(e: A): Boolean = entries.contains(e) && !tombstones.contains(e)

  override lazy val value: Set[A] = entries -- tombstones
}

object TwoPSet {
  def apply[A](values: A*): TwoPSet[A] = {
    new TwoPSet(values.toSet, Set())
  }

  implicit def TwoPSetCRDTInstance[A]: Lattice[TwoPSet[A]] = new Lattice[TwoPSet[A]] {
    override def merge(left: TwoPSet[A], right: TwoPSet[A]): TwoPSet[A] = {
      val e = left.entries ++ right.entries
      val t = left.tombstones ++ right.tombstones
      new TwoPSet(e, t)
    }
  }
}
