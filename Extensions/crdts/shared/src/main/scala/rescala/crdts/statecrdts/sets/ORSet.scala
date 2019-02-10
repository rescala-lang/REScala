package rescala.crdts.statecrdts
package sets

import rescala.crdts.statecrdts.sets.ORSet.Identifier

/**
  * Implementation of an Observed-Remove Set as described by Shapiro et al. (2011)
  *
  * @param payload The internal state of the set, consisting of two sets. One to store entries and their identifiers and one to track removed entries (tombstones).
  * @tparam A The type of the elements stored in this set
  */
//TODO: maybe use a Map[A,List(Identifier)] for entries. This would speed up removes but slow down adds
case class ORSet[A](payload: (Set[(A, Identifier)], Set[Identifier])) extends RemovableCRDTSet[A] {
  val (entries, tombstones) = payload

  override def add(a: A): ORSet[A] = {
    ORSet((entries + ((a, GenIDUtil.genId)), tombstones))
  }

  override def remove(a: A): ORSet[A] = {
    // fetch ids of all instances of the element
    val (_, newTombs) = entries.filter(entry => entry._1 == a).unzip
    ORSet((entries, tombstones ++ newTombs)) // add them to tombstones
  }

  override def contains(a: A): Boolean = value(a)

  override def value: Set[A] = {
    // filter all entries with tombstones
    val (values, _) = entries.filter(e => !tombstones(e._2)).unzip
    values
  }
}

object ORSet {
  type Identifier = String

  def apply[A](values: A*): ORSet[A] = {
    val a = values.map(a => (a, GenIDUtil.genId))
    new ORSet((a.toSet, Set()))
  }

  /** Allows the creation of new CRDTs by passing an initial value.
    *
    * @param value the value
    * @return new CRDT instance representing the value
    */
  def apply[A](value: Set[A]): ORSet[A] = {
    apply(value.toSeq: _*)
  }

  implicit def ORSetCRDTInstance[A]: StateCRDT[Set[A], ORSet[A]] = new StateCRDT[Set[A], ORSet[A]] {
    override def value(target: ORSet[A]): Set[A] = target.value

    override def merge(left: ORSet[A], right: ORSet[A]): ORSet[A] = {
      val (entries1, tombs1) = left.payload
      val (entries2, tombs2) = right.payload
      val (entries, tombstones) = (entries1 ++ entries2, tombs1 ++ tombs2)
      ORSet((entries, tombstones))
    }


  }
}
