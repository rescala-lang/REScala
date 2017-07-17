package stateCrdts

import scala.collection.immutable.HashMap

trait StateCRDT {
  type selfType <: StateCRDT
  type valueType
  type payloadType

  /**
    * the public state of the CRDT
    */
  def value: valueType

  /**
    * the internal state of the CRDT (payload)
    */
  def payload: payloadType

  /**
    * Merge this instance with another CRDT instance and return the resulting CRDT.
    *
    * @param c the other CRDT
    * @return a new CRDT instance representing a merge of the two former instances
    */
  def merge(c: StateCRDT): selfType

  def +(crdt: StateCRDT): selfType = merge(crdt)

  override def toString: String = value.toString

  /**
    * Constructs a new instance of this CRDT from a given payload
    *
    * @param payload the initial payload for this crdt
    * @return a new crdt instance with the given payload
    */
  def fromPayload(payload: payloadType): selfType

  def fromValue(value: valueType): selfType
}

trait StateCRDTSet extends StateCRDT {
  type ElementType

  def add(e: ElementType): selfType

  def remove(e: ElementType): selfType

  def contains(e: ElementType): Boolean
}

case class CIncOnlyCounter(id: String, payload: HashMap[String, Int]) extends StateCRDT {
  type selfType = CIncOnlyCounter
  type valueType = Int
  type payloadType = HashMap[String, Int]

  def value: valueType = payload.values.sum

  def merge(c: StateCRDT): CIncOnlyCounter = c match {
    case CIncOnlyCounter(i, p) => CIncOnlyCounter(id,
      payload.merged(p) {
        case ((k, v1), (_, v2)) => (k, v1 max v2)
      })
  }

  override def fromValue(value: Int): CIncOnlyCounter = CIncOnlyCounter(id, HashMap(id -> value))

  def fromPayload(payload: HashMap[String, Int]): CIncOnlyCounter = CIncOnlyCounter(id, payload)

  def increase = CIncOnlyCounter(id, payload + (id -> (payload(id) + 1)))
}

object CIncOnlyCounter {
  def apply(value: Int): CIncOnlyCounter = {
    val id = DistributionEngine.genId // assign random id based on host
    CIncOnlyCounter(id, HashMap(id -> value))
  }
}

/**
  * Implementation of an Observed-Remove Set as described by Shapiro et al. (2011)
  *
  * @param payload The internal state of the set, consisting of two sets. One to store entries and their identifiers and one to track removed entries (tombstones).
  * @tparam A The type of the elements stored in this set
  */
case class ORSet[A](payload: (Set[(A, Identifier)], Set[Identifier])) extends StateCRDTSet {
  override type selfType = ORSet[A]
  override type valueType = Set[A]
  override type payloadType = (Set[(A, Identifier)], Set[Identifier])
  override type ElementType = A
  val (entries, tombstones) = payload

  override def value: valueType = {
    val (values, _) = entries.filter(e => !tombstones(e._2)).unzip // filter all entries with tombstones
    values
  }

  override def merge(c: StateCRDT): ORSet[A] = c match {
    case o: ORSet[A] =>
      val (entries1, tombs1) = payload
      val (entries2, tombs2) = o.payload
      val (entries, tombstones) = (entries1 ++ entries2, tombs1 ++ tombs2)
      fromPayload((entries, tombstones))
  }

  override def fromPayload(payload: payloadType): ORSet[A] = ORSet(payload)

  override def add(e: A): ORSet[A] = {
    ORSet((entries + ((e, DistributionEngine.genId)), tombstones))
  }

  override def remove(e: A): ORSet[A] = {
    val (_, newTombs) = entries.filter(entry => entry._1 == e).unzip // fetch ids of all instances of the element
    ORSet((entries, tombstones ++ newTombs)) // add them to tombstones
  }

  override def contains(e: A): Boolean = value(e)

  override def fromValue(value: Set[A]): ORSet[A] = {
    val entries = value.map((a) => (a, DistributionEngine.genId))
    new ORSet((entries, Set()))
  }
}

object ORSet {
  def apply[A](values: A*): ORSet[A] = {
    val a = values.map((a) => (a, DistributionEngine.genId))
    new ORSet((a.toSet, Set()))
  }
}

