package rdts.datatypes.alternatives

import rdts.base.{Lattice, Uid}
import rdts.dotted.FilteredLattice

/** Implementation of an Observed-Remove Set similar to the one described by Shapiro et al. (2011) */
case class ObserveRemoveSet[A](entries: Map[Uid, A], tombstones: Set[Uid]) {

  def add(a: A): ObserveRemoveSet[A] = ObserveRemoveSet(Map(Uid.gen() -> a), Set.empty)

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
  def apply[A](values: Set[A]): ObserveRemoveSet[A] = ObserveRemoveSet(values.map(Uid.gen() -> _).toMap, Set())
  given lattice[A]: FilteredLattice[ObserveRemoveSet[A]]({
    given Lattice[A] = Lattice.assertEquals
    Lattice.derived
  }) with {
    override def filter(base: ObserveRemoveSet[A], other: ObserveRemoveSet[A]): ObserveRemoveSet[A] =
      ObserveRemoveSet(base.entries -- other.tombstones, base.tombstones)
  }
}
