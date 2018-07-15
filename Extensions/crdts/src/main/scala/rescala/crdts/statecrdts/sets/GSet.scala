package rescala.crdts.statecrdts
package sets

case class GSet[A](payload: Set[A]) extends StateCRDTSet[A] {
  override def add(e: A): GSet[A] = GSet(payload + e)

  override def contains(e: A): Boolean = payload.contains(e)

  def value: Set[A] = payload
}

object GSet {
  /**
    * Allows the creation of GSets with initial values.
    */
  def apply[A](values: A*): GSet[A] = {
    new GSet(values.toSet)
  }

  implicit def GSetStateCRDTInstance[A]: StateCRDT[Set[A], GSet[A]] = new StateCRDT[Set[A], GSet[A]] {
    override def value(target: GSet[A]): Set[A] = target.value

    override def merge(left: GSet[A], right: GSet[A]): GSet[A] = GSet(left.value.union(right.value))

    /** Allows the creation of new CRDTs by passing an initial value.
      *
      * @param value the value
      * @return new CRDT instance representing the value
      */
    override def fromValue(value: Set[A]): GSet[A] = GSet(value)

    /** Allows the creation of new CRDTs by passing a payload.
      *
      * @param payload the payload
      * @return new CRDT instance with the given payload
      */
    override def fromPayload[P](payload: P): GSet[A] = GSet(payload.asInstanceOf[Set[A]])
  }
}
