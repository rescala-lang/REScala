package rescala.crdts.statecrdts

import java.net.InetAddress

trait StateCRDT[A, F] {

  /** the public state of the CRDT */
  def value(target: F): A

  /** Merge this instance with another CRDT instance and return the resulting CRDT. */
  def merge(left: F, right: F): F
}

object StateCRDT {
  /**
    * Generates unique identifiers based on the current Hostname, IP address and a UUID based on the current system time.
    *
    * @return A new unique identifier (e.g. hostname/127.0.0.1::1274f9fe-cdf7-3f10-a7a4-33e8062d7435)
    */
  def genId: String = InetAddress.getLocalHost + "::" + java.util.UUID.nameUUIDFromBytes(BigInt(System.currentTimeMillis).toByteArray)
}
