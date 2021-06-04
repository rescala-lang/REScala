package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.crdt.GListCRDT
import rescala.extra.lattices.delta.crdt.GListCRDT.GListAsUIJDLattice

class GList[E](crdt: DeltaCRDT[GListCRDT.State[E]]) {
  def read(i: Int): Option[E] = crdt.query(GListCRDT.read(i))

  def toList: List[E] = crdt.query(GListCRDT.toList)

  def size: Int = crdt.query(GListCRDT.size)

  def insert(i: Int, e: E): GList[E] = new GList(crdt.mutate(GListCRDT.insert(i, e)))

  def processReceivedDeltas(): GList[E] = new GList(crdt.processReceivedDeltas())
}

object GList {
  type State[E] = GListCRDT.State[E]

  def apply[E](antiEntropy: AntiEntropy[State[E]]): GList[E] =
    new GList(DeltaCRDT.empty(antiEntropy))
}
