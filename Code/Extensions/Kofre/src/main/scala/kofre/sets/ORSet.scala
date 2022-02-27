package kofre.sets

import kofre.{Defs, Lattice}

/** Implementation of an Observed-Remove Set similar to the one described by Shapiro et al. (2011) */
case class ORSet[A](entries: Map[Defs.Id, A], tombstones: Set[Defs.Id]) {

  def add(a: A): ORSet[A] = ORSet(Map(Defs.genId() -> a), Set.empty)

  def remove(a: A): ORSet[A] = {
    // fetch ids of all instances of the element
    val (remove, keep) = entries.partition(_._2 == a)
    ORSet(Map.empty, remove.keySet)
  }

  def contains(a: A): Boolean = entries.values.exists(_ == a)

  def value: Set[A] = entries.values.toSet
}

object ORSet {
  def empty[A]: ORSet[A]                 = ORSet(Map.empty, Set.empty)
  def apply[A](values: Set[A]): ORSet[A] = ORSet(values.map(Defs.genId() -> _).toMap, Set())
  given lattice[A]: Lattice[ORSet[A]] = (left, right) =>
    val lefte  = left.entries -- right.tombstones
    val righte = right.entries -- left.tombstones
    ORSet(lefte ++ righte, left.tombstones ++ right.tombstones)
}
