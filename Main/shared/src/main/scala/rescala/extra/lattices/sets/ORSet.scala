package rescala.extra.lattices.sets

import rescala.extra.lattices.{IdUtil, Lattice}

/**
  * Implementation of an Observed-Remove Set similar to the one described by Shapiro et al. (2011)
  */
case class ORSet[A](entries: Map[IdUtil.Id, A], tombstones: Set[IdUtil.Id]) {

  def add(a: A): ORSet[A] = ORSet(entries.updated(IdUtil.genId, a), tombstones)

  def remove(a: A): ORSet[A] = {
    // fetch ids of all instances of the element
    val (remove, keep) = entries.partition(_._2 == a)
    ORSet(keep, tombstones ++ remove.keySet)
  }

  def contains(a: A): Boolean = entries.values.exists(_ == a)

  def value: Set[A] = entries.values.toSet
}

object ORSet {

  def empty[A]: ORSet[A] = ORSet(Map.empty, Set.empty)

  def apply[A](values: Set[A]): ORSet[A] = ORSet(values.map(IdUtil.genId -> _).toMap, Set())


  implicit def lattice[A]: Lattice[ORSet[A]] = new Lattice[ORSet[A]] {
    override def merge(left: ORSet[A], right: ORSet[A]): ORSet[A] = {
      val lefte = left.entries -- right.tombstones
      val righte = right.entries -- left.tombstones
      ORSet(lefte ++ righte, left.tombstones ++ right.tombstones)
    }
  }
}
