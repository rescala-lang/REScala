package statecrdts

import java.net.InetAddress

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

object StateCRDT {
  /**
    * Generates unique identifiers based on the current Hostname, IP address and a UUID based on the current system time.
    *
    * @return A new unique identifier (e.g. hostname/127.0.0.1::1274f9fe-cdf7-3f10-a7a4-33e8062d7435)
    */
  def genId: String = InetAddress.getLocalHost + "::" + java.util.UUID.nameUUIDFromBytes(BigInt(System.currentTimeMillis).toByteArray)
}