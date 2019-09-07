package rescala.lattices.sets

trait SetLike[A, F] {
  def add(set: F, value: A): F
  def contains(set: F, value: A): Boolean
}

object SetLike {
  implicit def setInstance[A]: SetLike[A, Set[A]] = new SetLike[A, Set[A]] {
    override def add(set: Set[A], value: A): Set[A] = set + value
    override def contains(set: Set[A], value: A): Boolean = set.contains(value)
  }
}
