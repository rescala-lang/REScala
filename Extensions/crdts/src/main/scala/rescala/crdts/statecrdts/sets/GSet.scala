package rescala.crdts.statecrdts
package sets

case class GSet[A](payload: Set[A]) extends StateCRDTSet[A] {
  override type selfType = GSet[A]
  override type payloadType = Set[A]

  override def add(e: A): GSet[A] = GSet(payload + e)

  override def contains(e: A): Boolean = payload.contains(e)

  override def merge(c: StateCRDT): GSet[A] = c.value match {
    case s: Set[A] => GSet(s.union(value))
  }

  override def value: Set[A] = payload

  override def fromPayload(payload: Set[A]): GSet[A] = GSet(payload)

  override def fromValue(value: Set[A]): GSet[A] = GSet(value)
}

object GSet {
  /**
    * Allows the creation of GSets with initial values.
    */
  def apply[A](values: A*): GSet[A] = {
    new GSet(values.toSet)
  }
}
