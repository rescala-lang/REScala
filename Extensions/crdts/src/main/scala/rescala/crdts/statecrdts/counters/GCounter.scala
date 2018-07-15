package rescala.crdts.statecrdts
package counters

import scala.collection.immutable.HashMap

case class GCounter(id: String, payload: HashMap[String, Int]) {
  def value: Int = payload.values.sum

  def fromValue(value: Int): GCounter = GCounter(id, HashMap(id -> value))

  def fromPayload(payload: HashMap[String, Int]): GCounter = GCounter(id, payload)

  def increase = GCounter(id, payload + (id -> (payload(id) + 1)))
}

object GCounter {
  def apply(value: Int): GCounter = {
    val id = StateCRDT.genId // assign random id based on host
    GCounter(id, HashMap(id -> value))
  }

  implicit def GCounterCRDT: StateCRDT[Int, GCounter] = new StateCRDT[Int, GCounter] {
    override def value(target: GCounter): Int = target.value

    override def merge(left: GCounter, right: GCounter): GCounter =
      GCounter(left.id,
        left.payload.merged(right.payload) {
          case ((k, v1), (_, v2)) => (k, v1 max v2)
        })

    /** Allows the creation of new CRDTs by passing an initial value.
      *
      * @param value the value
      * @return new CRDT instance representing the value
      */
    override def fromValue(value: Int): GCounter = GCounter(value)

    /** Allows the creation of new CRDTs by passing a payload.
      *
      * @param payload the payload
      * @return new CRDT instance with the given payload
      */
    override def fromPayload[P](payload: P): GCounter = GCounter(StateCRDT.genId,
      payload.asInstanceOf[HashMap[String, Int]])
  }

}
