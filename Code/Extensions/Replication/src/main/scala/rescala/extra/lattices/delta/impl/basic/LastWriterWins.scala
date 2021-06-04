package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.{CContext, TimedVal}
import rescala.extra.lattices.delta.crdt.LastWriterWinsCRDT

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
