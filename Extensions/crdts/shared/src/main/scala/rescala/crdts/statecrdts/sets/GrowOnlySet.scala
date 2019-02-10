package rescala.crdts.statecrdts
package sets

case class GrowOnlySet[A](payload: Set[A]) extends StateCRDTSet[A] {
  override def add(e: A): GrowOnlySet[A] = GrowOnlySet(payload + e)

  override def contains(e: A): Boolean = payload.contains(e)

  def value: Set[A] = payload
}

object GrowOnlySet {
  /**
    * Allows the creation of GSets with initial values.
    */
  def apply[A](values: A*): GrowOnlySet[A] = {
    new GrowOnlySet(values.toSet)
  }

  implicit def GSetStateCRDTInstance[A]: StateCRDT[Set[A], GrowOnlySet[A]] = new StateCRDT[Set[A], GrowOnlySet[A]] {
    override def value(target: GrowOnlySet[A]): Set[A] = target.value

    override def merge(left: GrowOnlySet[A], right: GrowOnlySet[A]): GrowOnlySet[A] = GrowOnlySet(left.value.union(right.value))

    /** Allows the creation of new CRDTs by passing an initial value.
      *
      * @param value the value
      * @return new CRDT instance representing the value
      */
    override def fromValue(value: Set[A]): GrowOnlySet[A] = GrowOnlySet(value)

    /** Allows the creation of new CRDTs by passing a payload.
      *
      * @param payload the payload
      * @return new CRDT instance with the given payload
      */
    override def fromPayload[P](payload: P): GrowOnlySet[A] = GrowOnlySet(payload.asInstanceOf[Set[A]])
  }
}
