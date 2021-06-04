package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.crdt.GSetCRDT

class GSet[E](crdt: DeltaCRDT[GSetCRDT.State[E]]) {
  def elements: Set[E] = crdt.query(GSetCRDT.elements)

  def insert(element: E): GSet[E] = new GSet(crdt.mutate(GSetCRDT.insert(element)))

  def processReceivedDeltas(): GSet[E] = new GSet(crdt.processReceivedDeltas())
}

object GSet {
  type State[E] = GSetCRDT.State[E]

  def apply[E](antiEntropy: AntiEntropy[State[E]]): GSet[E] =
    new GSet(DeltaCRDT.empty(antiEntropy))
}
