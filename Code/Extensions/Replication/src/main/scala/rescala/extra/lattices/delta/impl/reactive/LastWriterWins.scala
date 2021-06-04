package rescala.extra.lattices.delta.impl.reactive

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.{CContext, Delta, TimedVal}
import rescala.extra.lattices.delta.crdt.LastWriterWinsCRDT

class LastWriterWins[A, C: CContext](val crdt: DeltaCRDT[LastWriterWins.State[A, C]])
    extends CRDTInterface[LastWriterWins.State[A, C]] {
  def read: Option[A] = crdt.query(LastWriterWinsCRDT.read)

  def write(v: A): LastWriterWins[A, C] = new LastWriterWins(crdt.mutate(LastWriterWinsCRDT.write(v)))

  def map(f: A => A): LastWriterWins[A, C] = new LastWriterWins(crdt.mutate(LastWriterWinsCRDT.map(f)))

  def clear(): LastWriterWins[A, C] = new LastWriterWins(crdt.mutate(LastWriterWinsCRDT.clear()))

  def applyDelta(delta: Delta[LastWriterWins.State[A, C]]): LastWriterWins[A, C] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this else new LastWriterWins(newCRDT)
  }

  def resetDeltaBuffer(): LastWriterWins[A, C] = new LastWriterWins(crdt.resetDeltaBuffer())
}

object LastWriterWins {
  type State[A, C] = LastWriterWinsCRDT.State[A, C]
  type Embedded[A] = DotFun[TimedVal[A]]

  def apply[A, C: CContext](replicaID: String): LastWriterWins[A, C] =
    new LastWriterWins(DeltaCRDT.empty(replicaID))
}
