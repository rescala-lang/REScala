package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.crdt.PNCounterCRDT

class PNCounter(crdt: DeltaCRDT[PNCounterCRDT.State]) {
  def value: Int = crdt.query(PNCounterCRDT.value)

  def inc(): PNCounter = new PNCounter(crdt.mutate(PNCounterCRDT.inc))

  def dec(): PNCounter = new PNCounter(crdt.mutate(PNCounterCRDT.dec))

  def processReceivedDeltas(): PNCounter = new PNCounter(crdt.processReceivedDeltas())
}

object PNCounter {
  type State = PNCounterCRDT.State

  def apply(antiEntropy: AntiEntropy[PNCounterCRDT.State]): PNCounter =
    new PNCounter(DeltaCRDT.empty(antiEntropy))
}
