package rescala.crdts.statecrdts
package sets

/**
  * Two phase set where elements can be added and removed but never added again.
  *
  * @param payload The payload consisting of one set for added entries and one set for removed entries (tombstones).
  * @tparam A The type of the elements in the set.
  */
case class TwoPSet[A](payload: (Set[A], Set[A])) extends RemovableCRDTSet[A] {
  val (entries, tombstones): (Set[A], Set[A]) = payload

  override def add(e: A): TwoPSet[A] = TwoPSet((entries + e, tombstones))

  override def remove(e: A): TwoPSet[A] = if (entries(e)) TwoPSet((entries, tombstones + e)) else this

  override def contains(e: A): Boolean = entries.contains(e) && !tombstones.contains(e)

  override def value: Set[A] = entries -- tombstones
}

object TwoPSet {
  def apply[A](values: A*): TwoPSet[A] = {
    new TwoPSet((values.toSet, Set()))
  }

  implicit def TwoPSetCRDTInstance[A]: StateCRDT[Set[A], TwoPSet[A]] = new StateCRDT[Set[A], TwoPSet[A]] {
    override def value(target: TwoPSet[A]): Set[A] = target.value
    override def merge(left: TwoPSet[A], right: TwoPSet[A]): TwoPSet[A] = {
      val e = left.entries ++ right.entries
      val t = left.tombstones ++ right.tombstones
      new TwoPSet((e, t))
    }

    /** Allows the creation of new CRDTs by passing an initial value.
      *
      * @param value the value
      * @return new CRDT instance representing the value
      */
    override def fromValue(value: Set[A]): TwoPSet[A] = TwoPSet((value, Set[A]()))
  }
}
