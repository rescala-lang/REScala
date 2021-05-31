package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta._

object LastWriterWinsCRDT {
  type State[A, C] = MVRegister.State[TimedVal[A], C]

  def read[A, C: CContext]: DeltaQuery[State[A, C], Option[A]] =
    MVRegisterCRDT.read[TimedVal[A], C].andThen(s => s.reduceOption(UIJDLattice[TimedVal[A]].merge).map(_.value))

  def write[A, C: CContext](v: A): DeltaMutator[State[A, C]] =
    (replicaID, state) => MVRegisterCRDT.write(TimedVal(v, replicaID)).apply(replicaID, state)

  def map[A, C: CContext](f: A => A): DeltaMutator[State[A, C]] = (replicaID, state) =>
    read[A, C].apply(state).map(f) match {
      case None    => UIJDLattice[State[A, C]].bottom
      case Some(v) => write(v).apply(replicaID, state)
    }

  def clear[A, C: CContext](): DeltaMutator[State[A, C]] = MVRegisterCRDT.clear
}

class LastWriterWins[A, C: CContext](crdt: DeltaCRDT[LastWriterWins.State[A, C]]) {
  def read: Option[A] = crdt.query(LastWriterWinsCRDT.read)

  def write(v: A): LastWriterWins[A, C] = new LastWriterWins(crdt.mutate(LastWriterWinsCRDT.write(v)))

  def clear(): LastWriterWins[A, C] = new LastWriterWins(crdt.mutate(LastWriterWinsCRDT.clear()))

  def processReceivedDeltas(): LastWriterWins[A, C] = new LastWriterWins(crdt.processReceivedDeltas())
}

object LastWriterWins {
  type State[A, C] = LastWriterWinsCRDT.State[A, C]
  type Embedded[A] = DotFun[TimedVal[A]]

  def apply[A, C: CContext](ae: AntiEntropy[State[A, C]]): LastWriterWins[A, C] =
    new LastWriterWins(DeltaCRDT.empty(ae))
}

class RLastWriterWins[A, C: CContext](val crdt: RDeltaCRDT[RLastWriterWins.State[A, C]])
    extends CRDTInterface[RLastWriterWins.State[A, C]] {
  def read: Option[A] = crdt.query(LastWriterWinsCRDT.read)

  def write(v: A): RLastWriterWins[A, C] = new RLastWriterWins(crdt.mutate(LastWriterWinsCRDT.write(v)))

  def map(f: A => A): RLastWriterWins[A, C] = new RLastWriterWins(crdt.mutate(LastWriterWinsCRDT.map(f)))

  def clear(): RLastWriterWins[A, C] = new RLastWriterWins(crdt.mutate(LastWriterWinsCRDT.clear()))

  def applyDelta(delta: Delta[RLastWriterWins.State[A, C]]): RLastWriterWins[A, C] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this else new RLastWriterWins(newCRDT)
  }

  def resetDeltaBuffer(): RLastWriterWins[A, C] = new RLastWriterWins(crdt.resetDeltaBuffer())
}

object RLastWriterWins {
  type State[A, C] = LastWriterWinsCRDT.State[A, C]
  type Embedded[A] = DotFun[TimedVal[A]]

  def apply[A, C: CContext](replicaID: String): RLastWriterWins[A, C] =
    new RLastWriterWins(RDeltaCRDT.empty(replicaID))
}
