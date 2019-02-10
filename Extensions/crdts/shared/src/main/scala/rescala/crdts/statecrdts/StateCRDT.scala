package rescala.crdts.statecrdts

trait StateCRDT[A, F] {
  /** the public state of the CRDT */
  def value(target: F): A

  /** Merge this instance with another CRDT instance and return the resulting CRDT. */
  def merge(left: F, right: F): F
}


