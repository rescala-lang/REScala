package rescala.extra.lattices.delta.impl.reactive

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.{CContext, Delta, UIJDLattice}
import rescala.extra.lattices.delta.crdt.MVRegisterCRDT
import rescala.extra.lattices.delta.crdt.MVRegisterCRDT.State

class MVRegister[A: UIJDLattice, C: CContext](val crdt: DeltaCRDT[MVRegisterCRDT.State[A, C]])
    extends CRDTInterface[MVRegisterCRDT.State[A, C]] {
  def read: Set[A] = crdt.query(MVRegisterCRDT.read)

  def write(v: A): MVRegister[A, C] = new MVRegister(crdt.mutate(MVRegisterCRDT.write(v)))

  def clear(): MVRegister[A, C] = new MVRegister(crdt.mutate(MVRegisterCRDT.clear))

  def applyDelta(delta: Delta[State[A, C]]): CRDTInterface[State[A, C]] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this else new MVRegister(newCRDT)
  }
}

object MVRegister {
  type State[A, C] = MVRegisterCRDT.State[A, C]
  type Embedded[A] = DotFun[A]

  def apply[A: UIJDLattice, C: CContext](replicaID: String): MVRegister[A, C] =
    new MVRegister(DeltaCRDT.empty[State[A, C]](replicaID))
}
