package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.CContext
import rescala.extra.lattices.delta.DotStore.{DotMap, DotSet}
import rescala.extra.lattices.delta.crdt.AWSetCRDT

class AWSet[E, C: CContext](crdt: DeltaCRDT[AWSetCRDT.State[E, C]]) {
  def elements: Set[E] = crdt.query(AWSetCRDT.elements)

  def add(e: E): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.add(e)))

  def remove(e: E): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.remove(e)))

  def removeBy(cond: E => Boolean): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.removeBy(cond)))

  def clear(): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.clear()))

  def processReceivedDeltas(): AWSet[E, C] = new AWSet(crdt.processReceivedDeltas())
}

object AWSet {
  type State[E, C] = AWSetCRDT.State[E, C]
  type Embedded[E] = DotMap[E, DotSet]

  def apply[E, C: CContext](antiEntropy: AntiEntropy[State[E, C]]): AWSet[E, C] =
    new AWSet(DeltaCRDT.empty(antiEntropy))
}
