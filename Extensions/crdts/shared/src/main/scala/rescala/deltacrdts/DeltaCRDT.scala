package rescala.deltacrdts

/** @tparam A type of the CRDT
  * @tparam D type of the delta mutation */
trait DeltaCRDT[A, D] {
  def applyΔ(crdt: A, delta: D): A
}

object DeltaCRDT {
  def apply[A,D](implicit ev: DeltaCRDT[A,D]): DeltaCRDT[A,D] = ev

  def applyΔ[A, D](crdt: A, delta: D)(implicit ev: DeltaCRDT[A, D]): A = apply[A, D].applyΔ(crdt, delta)
}
