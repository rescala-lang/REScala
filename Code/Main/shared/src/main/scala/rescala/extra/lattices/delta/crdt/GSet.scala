package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.UIJDLatticeWithBottom.SetAsUIJDLattice

object GSetCRDT {
  type State[E] = Set[E]

  def apply[E](replicaID: String): DeltaCRDT[State[E]] =
    DeltaCRDT.empty[Set[E]](replicaID)

  def elements[E]: DeltaQuery[State[E], Set[E]] = state => state

  def insert[E](element: E): DeltaMutator[State[E]] = (_, _) => Set(element)
}

class GSet[E](crdt: DeltaCRDT[GSetCRDT.State[E]]) {
  def elements: Set[E] = crdt.query(GSetCRDT.elements)

  def insert(element: E): GSet[E] = new GSet(crdt.mutate(GSetCRDT.insert(element)))
}

object GSet {
  def apply[E](replicaID: String): GSet[E] = new GSet(GSetCRDT[E](replicaID))
}
