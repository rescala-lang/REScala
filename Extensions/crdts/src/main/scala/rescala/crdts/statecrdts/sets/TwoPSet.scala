package rescala.crdts.statecrdts
package sets

/**
  * Two phase set where elements can be added and removed but never added again.
  *
  * @param payload The payload consisting of one set for added entries and one set for removed entries (tombstones).
  * @tparam A The type of the elements in the set.
  */
case class TwoPSet[A](payload: (Set[A], Set[A])) extends RemovableStateCRDTSet[A] {
  override type selfType = TwoPSet[A]
  override type payloadType = (Set[A], Set[A])
  val (entries, tombstones): (Set[A], Set[A]) = payload

  override def add(e: A): selfType = TwoPSet((entries + e, tombstones))

  override def remove(e: A): selfType = if (entries(e)) TwoPSet((entries, tombstones + e)) else this

  override def contains(e: A): Boolean = entries.contains(e) && !tombstones.contains(e)

  override def value: valueType = entries -- tombstones

  override def merge(c: StateCRDT): selfType = c match {
    case s: TwoPSet[A] =>
      val e = entries ++ s.entries
      val t = tombstones ++ s.tombstones
      new TwoPSet((e, t))
  }

  override def fromPayload(payload: payloadType): selfType = TwoPSet(payload)

  override def fromValue(value: valueType): selfType = TwoPSet((value, Set[A]()))
}

object TwoPSet {
  def apply[A](values: A*): TwoPSet[A] = {
    new TwoPSet((values.toSet, Set()))
  }
}
