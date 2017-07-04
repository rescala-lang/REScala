import scala.collection.immutable.HashMap

trait StateCRDT[A, B, C <: StateCRDT[A, B, C]] {
  this: C => // self - type

  /**
    * the public state of the CRDT
    */
  def value: A

  /**
    * the internal state of the CRDT (payload)
    */
  def payload: B

  /**
    * Merge two instances of this CRDT
    *
    * @param CRDT to be merged with this one
    * @return the resulting CRDT
    */
  def merge(crdt: C): C

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


// c: rescala.Signal[Int] = <console>(17)

case class CIncOnlyCounter(id: String, payload: HashMap[String, Int]) extends StateCRDT[Int, HashMap[String, Int], CIncOnlyCounter] {
  def merge(c: CIncOnlyCounter) = CIncOnlyCounter(id,
    payload.merged(c.payload) {
      case ((k, v1), (_, v2)) => (k, v1 max v2)
    })

  def value: Int = payload.values.sum

  def increment: CIncOnlyCounter = this + 1

  def +(i: Int): CIncOnlyCounter = CIncOnlyCounter(id, payload + (id -> (payload(id) + i)))

  def fromPayload(payload: HashMap[String, Int]): StateCRDT[Int, HashMap[String, Int], CIncOnlyCounter] = CIncOnlyCounter(id, payload)
}

object CIncOnlyCounter {
  def apply(value: Int): CIncOnlyCounter = {
    val id = DistributionEngine.host + java.util.UUID.randomUUID.toString // assign random id based on host
    new CIncOnlyCounter(id, HashMap(id -> value))
  }
}