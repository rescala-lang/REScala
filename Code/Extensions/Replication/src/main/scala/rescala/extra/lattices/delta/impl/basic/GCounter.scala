package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.crdt.GCounterCRDT

class GCounter(crdt: DeltaCRDT[GCounterCRDT.State]) {
  def value: Int = crdt.query(GCounterCRDT.value)

  def inc(): GCounter = new GCounter(crdt.mutate(GCounterCRDT.inc()))

  def processReceivedDeltas(): GCounter = new GCounter(crdt.processReceivedDeltas())
}

object GCounter {
  type State = GCounterCRDT.State

  def apply(antiEntropy: AntiEntropy[State]): GCounter =
    new GCounter(DeltaCRDT.empty(antiEntropy))
}
