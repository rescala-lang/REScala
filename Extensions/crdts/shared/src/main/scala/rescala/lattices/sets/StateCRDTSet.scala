package rescala.lattices.sets

trait StateCRDTSet[A] {
  def add(e: A): StateCRDTSet[A]

  def value: Set[A]
  def contains(e: A): Boolean
}
