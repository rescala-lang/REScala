package kofre.datatypes.alternatives.rga

import kofre.datatypes.TwoPhaseSet

trait SetLike[A, F] {
  def add(set: F, value: A): F
  def contains(set: F, value: A): Boolean
}

object SetLike {
  given setLike[A]: SetLike[A, Set[A]] = new:
    override def add(set: Set[A], value: A): Set[A]       = set + value
    override def contains(set: Set[A], value: A): Boolean = set.contains(value)

  given twoPSetLike[A]: SetLike[A, TwoPhaseSet[A]] = new:
    override def add(set: TwoPhaseSet[A], value: A): TwoPhaseSet[A] = set.insert(value)
    override def contains(set: TwoPhaseSet[A], value: A): Boolean   = set.contains(value)
}
