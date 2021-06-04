package rescala.extra.lattices.delta.impl.reactive

import rescala.extra.lattices.delta.Delta
import rescala.extra.lattices.delta.crdt.GListCRDT
import rescala.extra.lattices.delta.crdt.GListCRDT.GListAsUIJDLattice

class GList[E](val crdt: DeltaCRDT[GListCRDT.State[E]]) extends CRDTInterface[GListCRDT.State[E]] {
  def read(i: Int): Option[E] = crdt.query(GListCRDT.read(i))

  def toList: List[E] = crdt.query(GListCRDT.toList)

  def size: Int = crdt.query(GListCRDT.size)

  def insert(i: Int, e: E): GList[E] = new GList(crdt.mutate(GListCRDT.insert(i, e)))

  def applyDelta(delta: Delta[GListCRDT.State[E]]): GList[E] = {
    val newCRDT = crdt.applyDelta(delta)

    if (newCRDT == crdt) this
    else new GList(newCRDT)
  }
}

object GList {
  type State[E] = GListCRDT.State[E]

  def apply[E](replicaID: String): GList[E] =
    new GList(DeltaCRDT.empty(replicaID))
}
