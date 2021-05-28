package rescala.extra.lattices.delta

abstract class CRDTInterface[A: UIJDLattice] {
  val crdt: RDeltaCRDT[A]

  def applyDelta(delta: Delta[A]): CRDTInterface[A]
}
