package statecrdts
package sets

import statecrdts.sets.ORSet.Identifier

/**
  * Implementation of an Observed-Remove Set as described by Shapiro et al. (2011)
  *
  * @param payload The internal state of the set, consisting of two sets. One to store entries and their identifiers and one to track removed entries (tombstones).
  * @tparam A The type of the elements stored in this set
  */
case class ORSet[A](payload: (Set[(A, Identifier)], Set[Identifier])) extends RemovableStateCRDTSet[A] { //TODO: maybe use a Map[A,List(Identifier)] for entries. This would speed up removes but slow down adds
override type selfType = ORSet[A]
  override type payloadType = (Set[(A, Identifier)], Set[Identifier])
  val (entries, tombstones) = payload

  override def merge(c: StateCRDT): ORSet[A] = c match {
    case o: ORSet[A] =>
      val (entries1, tombs1) = payload
      val (entries2, tombs2) = o.payload
      val (entries, tombstones) = (entries1 ++ entries2, tombs1 ++ tombs2)
      fromPayload((entries, tombstones))
  }

  override def fromPayload(payload: payloadType): ORSet[A] = ORSet(payload)

  override def add(a: A): ORSet[A] = {
    ORSet((entries + ((a, StateCRDT.genId)), tombstones))
  }

  override def remove(a: A): ORSet[A] = {
    val (_, newTombs) = entries.filter(entry => entry._1 == a).unzip // fetch ids of all instances of the element
    ORSet((entries, tombstones ++ newTombs)) // add them to tombstones
  }

  override def contains(a: A): Boolean = value(a)

  override def value: valueType = {
    val (values, _) = entries.filter(e => !tombstones(e._2)).unzip // filter all entries with tombstones
    values
  }

  override def fromValue(value: Set[A]): ORSet[A] = {
    val entries = value.map((a) => (a, StateCRDT.genId))
    new ORSet((entries, Set()))
  }
}

object ORSet {
  type Identifier = String

  def apply[A](values: A*): ORSet[A] = {
    val a = values.map((a) => (a, StateCRDT.genId))
    new ORSet((a.toSet, Set()))
  }
}