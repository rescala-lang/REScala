package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.crdt.TwoPSetCRDT

class TwoPSet[E](crdt: DeltaCRDT[TwoPSetCRDT.State[E]]) {
  def elements: Set[E] = crdt.query(TwoPSetCRDT.elements)

  def insert(element: E): TwoPSet[E] = new TwoPSet(crdt.mutate(TwoPSetCRDT.insert(element)))

  def remove(element: E): TwoPSet[E] = new TwoPSet(crdt.mutate(TwoPSetCRDT.remove(element)))

  def processReceivedDeltas(): TwoPSet[E] = new TwoPSet(crdt.processReceivedDeltas())
}

object TwoPSet {
  type State[E] = TwoPSetCRDT.State[E]

  def apply[E](antiEntropy: AntiEntropy[State[E]]): TwoPSet[E] =
    new TwoPSet(DeltaCRDT.empty(antiEntropy))
}
