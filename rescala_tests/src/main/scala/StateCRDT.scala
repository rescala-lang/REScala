import scala.collection.immutable.HashMap

abstract class StateCRDT[A, B, C] {this: C =>

  /**
    * the public state of the CRDT
    */
  def value: A

  /**
    * the internal state of the CRDT (payload)
    */
  def payload: B

  /**
    *
    * @param c
    * @return
    */
  def merge(c: C): C

  def +(crdt: C): C = merge(crdt)

  override def toString: String = value.toString

  /**
    * Constructs a new instance of this CRDT from a given payload
    *
    * @param payload
    * @return
    */
  def fromPayload(payload: B): C
}

case class CIncOnlyCounter(id: String, payload: HashMap[String, Int]) extends StateCRDT[Int, HashMap[String, Int], CIncOnlyCounter] {
  def value = payload.values.sum

  def merge(c: CIncOnlyCounter) = c match {
    case CIncOnlyCounter(i, p) => CIncOnlyCounter(id,
      payload.merged(p) {
        case ((k, v1), (_, v2)) => (k, v1 max v2)
      })
  }

  override def fromPayload(payload: HashMap[String, Int]): CIncOnlyCounter = CIncOnlyCounter(payload)
}
object CIncOnlyCounter {
  def genId = DistributionEngine.host+java.util.UUID.randomUUID.toString
  def apply(value: Int): CIncOnlyCounter = {
    //val id = DistributionEngine.host + java.util.UUID.randomUUID.toString // assign random id based on host
    val id = genId // assign random id based on host
    CIncOnlyCounter(id, HashMap(id -> value))
  }
  def apply(payload: HashMap[String, Int]): CIncOnlyCounter = {
    CIncOnlyCounter(genId, payload)
  }
}

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