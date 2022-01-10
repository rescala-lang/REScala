package kofre.sets

import kofre.Lattice

/** Two phase set where elements can be added and removed but never added again. */
case class TwoPSet[A](entries: Set[A], tombstones: Set[A]) {

  def add(e: A): TwoPSet[A] =
    if (!tombstones.contains(e)) TwoPSet(entries + e, tombstones)
    else this
  def add(es: Set[A]): TwoPSet[A] = TwoPSet(entries ++ (es -- tombstones), tombstones)

  def remove(e: A): TwoPSet[A] = if (entries(e)) TwoPSet(entries - e, tombstones + e) else this
  def remove(es: Set[A]): TwoPSet[A] = {
    val contained = entries.intersect(es)
    TwoPSet(entries -- contained, tombstones ++ contained)
  }

  def contains(e: A): Boolean = entries.contains(e) && !tombstones.contains(e)

  lazy val value: Set[A] = entries -- tombstones
}

object TwoPSet {
  def apply[A](values: A*): TwoPSet[A] = {
    new TwoPSet(values.toSet, Set())
  }

  given lattice[A]: Lattice[TwoPSet[A]] = (left, right) =>
    left.add(right.entries).remove(right.tombstones)
}
