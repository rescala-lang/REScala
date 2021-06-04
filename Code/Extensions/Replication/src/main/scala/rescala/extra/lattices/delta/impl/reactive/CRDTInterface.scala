package rescala.extra.lattices.delta.impl.reactive

import rescala.extra.lattices.delta.{Delta, UIJDLattice}

abstract class CRDTInterface[A: UIJDLattice] {
  val crdt: DeltaCRDT[A]

  def applyDelta(delta: Delta[A]): CRDTInterface[A]
}
