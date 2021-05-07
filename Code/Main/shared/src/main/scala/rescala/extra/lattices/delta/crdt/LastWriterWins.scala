package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.{CContext, Delta, RDeltaCRDT, TimedVal}

object LastWriterWinsCRDT {
  type State[A, C] = ORValueCRDT.State[TimedVal[A], C]

  def read[A, C: CContext]: DeltaQuery[State[A, C], Option[A]] =
    ORValueCRDT.read[TimedVal[A], C].andThen(_.map(tv => tv.value))

  def write[A, C: CContext](v: A): DeltaMutator[State[A, C]] = (replicaID, state) => {
    val m = ORValueCRDT.write(TimedVal(v, replicaID))
    m(replicaID, state)
  }

  def clear[A, C: CContext](): DeltaMutator[State[A, C]] = ORValueCRDT.clear[TimedVal[A], C]()
}

class LastWriterWins[A, C: CContext](val crdt: RDeltaCRDT[LastWriterWins.State[A, C]])
    extends CRDTInterface[LastWriterWins.State[A, C]] {
  def read: Option[A] = crdt.query(LastWriterWinsCRDT.read)

  def write(v: A): LastWriterWins[A, C] = new LastWriterWins(crdt.mutate(LastWriterWinsCRDT.write(v)))

  def clear(): LastWriterWins[A, C] = new LastWriterWins(crdt.mutate(LastWriterWinsCRDT.clear()))

  def applyDelta(delta: Delta[LastWriterWins.State[A, C]]): LastWriterWins[A, C] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this else new LastWriterWins(newCRDT)
  }
}

object LastWriterWins {
  type State[A, C] = LastWriterWinsCRDT.State[A, C]
  type Embedded[A] = DotFun[TimedVal[A]]

  def apply[A, C: CContext](replicaID: String): RDeltaCRDT[State[A, C]] =
    RDeltaCRDT.empty[State[A, C]](replicaID)
}
