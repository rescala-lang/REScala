import sun.swing.SwingUtilities2.AATextInfo

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
    *
    * @param c
    * @return
    */
  def merge(c: StateCRDT): selfType

  //def +(crdt: StateCRDT): StateCRDT = merge(crdt)

  override def toString: String = value.toString

  /**
    * Constructs a new instance of this CRDT from a given payload
    *
    * @param payload
    * @return
    */
  def fromPayload(payload: payloadType): selfType
}

case class CIncOnlyCounter(id: String, payload: HashMap[String, Int]) extends StateCRDT {
  type selfType = CIncOnlyCounter
  type valueType = Int
  type payloadType = HashMap[String, Int]

  def value = payload.values.sum

  def merge(c: StateCRDT) = c match {
    case CIncOnlyCounter(i, p) => CIncOnlyCounter(id,
      payload.merged(p) {
        case ((k, v1), (_, v2)) => (k, v1 max v2)
      })
  }


  def fromPayload(payload: HashMap[String, Int]): CIncOnlyCounter = CIncOnlyCounter(payload)

  def increase = CIncOnlyCounter(id, payload + (id -> (payload(id) + 1)))

  /**
    * Constructs a new instance of this CRDT from a given payload
    *
    * @param payload
    * @return
    */
  /*
  override def fromPayload(payload: payloadType): CIncOnlyCounter = {
    val p: HashMap[String, Int] = payload
    CIncOnlyCounter(payload)
  }
  */
}

object CIncOnlyCounter {
  def genId = DistributionEngine.host + java.util.UUID.randomUUID.toString

  def apply(value: Int): CIncOnlyCounter = {
    //val id = DistributionEngine.host + java.util.UUID.randomUUID.toString // assign random id based on host
    val id = genId // assign random id based on host
    CIncOnlyCounter(id, HashMap(id -> value))
  }

  def apply(payload: HashMap[String, Int]): CIncOnlyCounter = {
    CIncOnlyCounter(genId, payload)
  }
}

/*
case class Foo(id: String, payload: HashMap[String, Float]) extends StateCRDT[Float, HashMap[String, Float], Foo] {
  def value = payload.values.sum

  def merge(c: Foo) = c match {
    case Foo(i, p) => Foo(id,
      payload.merged(p) {
        case ((k, v1), (_, v2)) => (k, v1 max v2)
      })
  }

  override def fromPayload(payload: HashMap[String, Float]): Foo = Foo(payload)
}
object Foo {
  def genId = java.util.UUID.randomUUID.toString
  def apply(value: Float): Foo = {
    val id = DistributionEngine.host+java.util.UUID.randomUUID.toString // assign random id based on host
    new Foo(id, HashMap(id -> value))
  }

  def apply(payload: HashMap[String, Float]): Foo = {
    new Foo(genId, payload)
  }
}
*/