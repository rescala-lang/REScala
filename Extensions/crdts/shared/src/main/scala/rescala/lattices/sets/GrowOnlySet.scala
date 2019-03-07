package rescala.lattices.sets

import rescala.lattices.Lattice

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

  implicit def GSetStateCRDTInstance[A]: Lattice[GrowOnlySet[A]] = new Lattice[GrowOnlySet[A]] {
    override def merge(left: GrowOnlySet[A], right: GrowOnlySet[A]): GrowOnlySet[A] = GrowOnlySet(left.value.union(right.value))
  }
}
