package statecrdts
package counters

import scala.collection.immutable.HashMap

case class GCounter(id: String, payload: HashMap[String, Int]) extends StateCRDT {
  type selfType = GCounter
  type valueType = Int
  type payloadType = HashMap[String, Int]

  def value: valueType = payload.values.sum

  def merge(c: StateCRDT): GCounter = c match {
    case GCounter(_, p) => GCounter(id,
      payload.merged(p) {
        case ((k, v1), (_, v2)) => (k, v1 max v2)
      })
  }

  override def fromValue(value: Int): GCounter = GCounter(id, HashMap(id -> value))

  def fromPayload(payload: HashMap[String, Int]): GCounter = GCounter(id, payload)

  def increase = GCounter(id, payload + (id -> (payload(id) + 1)))
}

object GCounter {
  def apply(value: Int): GCounter = {
    val id = StateCRDT.genId // assign random id based on host
    GCounter(id, HashMap(id -> value))
  }
}