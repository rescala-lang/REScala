package rescala.lattices.sets

import rescala.lattices.sets.ORSet.Identifier
import rescala.lattices.{IdUtil, Lattice}

/**
  * Implementation of an Observed-Remove Set as described by Shapiro et al. (2011)
  *
  * @param payload The internal state of the set, consisting of two sets. One to store entries and their identifiers and one to track removed entries (tombstones).
  * @tparam A The type of the elements stored in this set
  */
//TODO: maybe use a Map[A,List(Identifier)] for entries. This would speed up removes but slow down adds
case class ORSet[A](entries: Set[(A, Identifier)], tombstones : Set[Identifier]) {

  def add(a: A): ORSet[A] = {
    ORSet(entries + ((a, IdUtil.genId)), tombstones)
  }

  def remove(a: A): ORSet[A] = {
    // fetch ids of all instances of the element
    val (_, newTombs) = entries.filter(entry => entry._1 == a).unzip
    ORSet(entries, tombstones ++ newTombs) // add them to tombstones
  }

  def contains(a: A): Boolean = value(a)

  def value: Set[A] = {
    // filter all entries with tombstones
    val (values, _) = entries.filter(e => !tombstones(e._2)).unzip
    values
  }
}

object ORSet {
  type Identifier = IdUtil.Id

  def empty[A]: ORSet[A] = ORSet(Set.empty, Set.empty)

  def apply[A](values: Set[A]): ORSet[A] = {
    val a = values.map(a => (a, IdUtil.genId))
    new ORSet(a, Set())
  }


  implicit def ORSetCRDTInstance[A]: Lattice[ORSet[A]] = new Lattice[ORSet[A]] {
    override def merge(left: ORSet[A], right: ORSet[A]): ORSet[A] =
      ORSet(left.entries ++ right.entries, left.tombstones ++ right.tombstones)
  }
}
