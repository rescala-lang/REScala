package rescala.crdts.statecrdts

trait StateCRDT[A, F] {
  /** the public state of the CRDT */
  def value(target: F): A

  /** Merge this instance with another CRDT instance and return the resulting CRDT. */
  def merge(left: F, right: F): F
}

object StateCRDT {
  /** Generates unique identifiers for use by CRDTs */
  def genId: String = java.util.UUID.randomUUID().toString
}
