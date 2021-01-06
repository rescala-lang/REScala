package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.{CContext, Causal, DeltaCRDT, UIJDLattice}
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore.DotFun

object MVRegisterCRDT {
  type State[A, C] = Causal[DotFun[A], C]

  def apply[A: UIJDLattice, C: CContext](replicaID: String): DeltaCRDT[State[A, C]] =
    DeltaCRDT.empty[State[A, C]](replicaID)

  def read[A, C: CContext]: DeltaQuery[State[A, C], Set[A]] = {
    case Causal(df, _) => df.values.toSet
  }

  def write[A: UIJDLattice, C: CContext](v: A): DeltaMutator[State[A, C]] = {
    case (replicaID, Causal(df, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      Causal(
        Map(nextDot -> v),
        CContext[C].fromSet(df.keySet + nextDot)
      )
  }

  def clear[A: UIJDLattice, C: CContext]: DeltaMutator[State[A, C]] = {
    case (_, Causal(df, _)) =>
      Causal(
        DotFun[A].empty,
        CContext[C].fromSet(df.keySet)
      )
  }
}

class MVRegister[A: UIJDLattice, C: CContext](crdt: DeltaCRDT[MVRegisterCRDT.State[A, C]]) {
  def read: Set[A] = crdt.query(MVRegisterCRDT.read)

  def write(v: A): MVRegister[A, C] = new MVRegister(crdt.mutate(MVRegisterCRDT.write(v)))

  def clear(): MVRegister[A, C] = new MVRegister(crdt.mutate(MVRegisterCRDT.clear))
}

object MVRegister {
  def apply[A: UIJDLattice, C: CContext](replicaID: String): MVRegister[A, C] =
    new MVRegister(MVRegisterCRDT[A, C](replicaID))
}
