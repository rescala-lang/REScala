package kofre.datatypes

import kofre.base.{Id, Lattice}

/** Implementation of an Observed-Remove Set similar to the one described by Shapiro et al. (2011) */
case class ObserveRemoveSet[A](entries: Map[Id, A], tombstones: Set[Id]) {

  def add(a: A): ObserveRemoveSet[A] = ObserveRemoveSet(Map(Id.genId() -> a), Set.empty)

  def remove(a: A): ObserveRemoveSet[A] = {
    // fetch ids of all instances of the element
    val (remove, keep) = entries.partition(_._2 == a)
    ObserveRemoveSet(Map.empty, remove.keySet)
  }

  def contains(a: A): Boolean = entries.values.exists(_ == a)

  def value: Set[A] = entries.values.toSet
}

object ObserveRemoveSet {
  def empty[A]: ObserveRemoveSet[A]                 = ObserveRemoveSet(Map.empty, Set.empty)
  def apply[A](values: Set[A]): ObserveRemoveSet[A] = ObserveRemoveSet(values.map(Id.genId() -> _).toMap, Set())
  given lattice[A]: Lattice[ObserveRemoveSet[A]] = (left, right) =>
    val lefte  = left.entries -- right.tombstones
    val righte = right.entries -- left.tombstones
    ObserveRemoveSet(lefte ++ righte, left.tombstones ++ right.tombstones)
}
