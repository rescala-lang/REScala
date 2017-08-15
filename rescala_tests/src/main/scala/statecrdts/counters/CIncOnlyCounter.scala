package statecrdts
package counters

import scala.collection.immutable.HashMap

case class CIncOnlyCounter(id: String, payload: HashMap[String, Int]) extends StateCRDT {
  type selfType = CIncOnlyCounter
  type valueType = Int
  type payloadType = HashMap[String, Int]

  def value: valueType = payload.values.sum

  def merge(c: StateCRDT): CIncOnlyCounter = c match {
    case CIncOnlyCounter(_, p) => CIncOnlyCounter(id,
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
    val id = StateCRDT.genId // assign random id based on host
    CIncOnlyCounter(id, HashMap(id -> value))
  }
}