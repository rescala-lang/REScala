package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.{CContext, UIJDLattice}
import rescala.extra.lattices.delta.crdt.MVRegisterCRDT

class MVRegister[A: UIJDLattice, C: CContext](crdt: DeltaCRDT[MVRegisterCRDT.State[A, C]]) {
  def read: Set[A] = crdt.query(MVRegisterCRDT.read)

  def write(v: A): MVRegister[A, C] = new MVRegister(crdt.mutate(MVRegisterCRDT.write(v)))

  def clear(): MVRegister[A, C] = new MVRegister(crdt.mutate(MVRegisterCRDT.clear))

  def processReceivedDeltas(): MVRegister[A, C] = new MVRegister(crdt.processReceivedDeltas())
}

object MVRegister {
  type State[A, C] = MVRegisterCRDT.State[A, C]
  type Embedded[A] = DotFun[A]

  def apply[A: UIJDLattice, C: CContext](antiEntropy: AntiEntropy[State[A, C]]): MVRegister[A, C] =
    new MVRegister(DeltaCRDT.empty[State[A, C]](antiEntropy))
}
