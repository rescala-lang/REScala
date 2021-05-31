package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta._
import rescala.extra.lattices.delta.crdt.MVRegisterCRDT.State

object MVRegisterCRDT {
  type State[A, C] = Causal[DotFun[A], C]

  private def deltaState[A: UIJDLattice, C: CContext](
      df: Option[DotFun[A]] = None,
      cc: Option[C]
  ): State[A, C] = {
    val bottom = UIJDLattice[State[A, C]].bottom

    Causal(
      df.getOrElse(bottom.dotStore),
      cc.getOrElse(bottom.cc)
    )
  }

  def read[A: UIJDLattice, C: CContext]: DeltaQuery[State[A, C], Set[A]] = {
    case Causal(df, _) => df.values.toSet
  }

  def write[A: UIJDLattice, C: CContext](v: A): DeltaMutator[State[A, C]] = {
    case (replicaID, Causal(df, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      deltaState(
        df = Some(Map(nextDot -> v)),
        cc = Some(CContext[C].fromSet(df.keySet + nextDot))
      )
  }

  def clear[A: UIJDLattice, C: CContext]: DeltaMutator[State[A, C]] = {
    case (_, Causal(df, _)) =>
      deltaState(
        cc = Some(CContext[C].fromSet(df.keySet))
      )
  }
}

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

class RMVRegister[A: UIJDLattice, C: CContext](val crdt: RDeltaCRDT[MVRegisterCRDT.State[A, C]])
    extends CRDTInterface[MVRegisterCRDT.State[A, C]] {
  def read: Set[A] = crdt.query(MVRegisterCRDT.read)

  def write(v: A): RMVRegister[A, C] = new RMVRegister(crdt.mutate(MVRegisterCRDT.write(v)))

  def clear(): RMVRegister[A, C] = new RMVRegister(crdt.mutate(MVRegisterCRDT.clear))

  def applyDelta(delta: Delta[State[A, C]]): CRDTInterface[State[A, C]] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this else new RMVRegister(newCRDT)
  }
}

object RMVRegister {
  type State[A, C] = MVRegisterCRDT.State[A, C]
  type Embedded[A] = DotFun[A]

  def apply[A: UIJDLattice, C: CContext](replicaID: String): RMVRegister[A, C] =
    new RMVRegister(RDeltaCRDT.empty[State[A, C]](replicaID))
}
