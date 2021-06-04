package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.crdt.LexCounterCRDT

class LexCounter(crdt: DeltaCRDT[LexCounterCRDT.State]) {
  def value: Int = crdt.query(LexCounterCRDT.value)

  def inc(): LexCounter = new LexCounter(crdt.mutate(LexCounterCRDT.inc()))

  def dec(): LexCounter = new LexCounter(crdt.mutate(LexCounterCRDT.dec()))

  def processReceivedDeltas(): LexCounter = new LexCounter(crdt.processReceivedDeltas())
}

object LexCounter {
  type State = LexCounterCRDT.State

  def apply(antiEntropy: AntiEntropy[State]): LexCounter =
    new LexCounter(DeltaCRDT.empty(antiEntropy))
}
