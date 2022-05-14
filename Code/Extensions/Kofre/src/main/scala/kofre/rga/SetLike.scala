package kofre.rga

import kofre.datatypes.TwoPSet

trait SetLike[A, F] {
  def add(set: F, value: A): F
  def contains(set: F, value: A): Boolean
}

object SetLike {
  given setLike[A]: SetLike[A, Set[A]] = new:
    override def add(set: Set[A], value: A): Set[A]       = set + value
    override def contains(set: Set[A], value: A): Boolean = set.contains(value)

  given twoPSetLike[A]: SetLike[A, TwoPSet[A]] = new:
    override def add(set: TwoPSet[A], value: A): TwoPSet[A]   = set.insert(value)
    override def contains(set: TwoPSet[A], value: A): Boolean = set.contains(value)
}
