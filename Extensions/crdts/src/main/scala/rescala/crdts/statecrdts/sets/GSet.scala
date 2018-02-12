package rescala.crdts.statecrdts
package sets

case class GSet[A](payload: Set[A]) extends StateCRDTSet[A] {
  type selfType = GSet[A]
  type payloadType = Set[A]

  override def add(e: A): GSet[A] = GSet(payload + e)

  override def contains(e: A): Boolean = payload.contains(e)

  def value: Set[A] = payload

  def fromPayload(payload: Set[A]): GSet[A] = GSet(payload)

  def fromValue(value: Set[A]): GSet[A] = GSet(value)
}

object GSet {
  /**
    * Allows the creation of GSets with initial values.
    */
  def apply[A](values: A*): GSet[A] = {
    new GSet(values.toSet)
  }

  implicit def GSetStateCRDTInstance[A] = new StateCRDT[Set[A], GSet[A]] {
    override def value(target: GSet[A]): Set[A] = target.value
    override def merge(left: GSet[A], right: GSet[A]): GSet[A] = GSet(left.value.union(right.value))
  }
}
