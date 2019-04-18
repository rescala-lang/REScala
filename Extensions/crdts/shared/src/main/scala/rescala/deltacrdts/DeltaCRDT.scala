package rescala.deltacrdts

/**
  *
  * @tparam A type of the CRDT
  * @tparam D type of the delta mutations
  */
trait DeltaCRDT[A, D] {
  /**
    * Applies a delta mutation to a CRDT
    * @return
    */
  def applyΔ(crdt: A, delta: D): A
}

object DeltaCRDT {
  def apply[A,D](implicit ev: DeltaCRDT[A,D]): DeltaCRDT[A,D] = ev
  def applyΔ[A,D](implicit ev: DeltaCRDT[A,D], crdt: A, delta: D): A = apply[A,D].applyΔ(crdt, delta)
}
