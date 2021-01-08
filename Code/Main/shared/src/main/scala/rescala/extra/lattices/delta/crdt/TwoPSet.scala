package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT
import rescala.extra.lattices.delta.DeltaCRDT.{DeltaMutator, DeltaQuery}


object TwoPSetCRDT {
  type State[E] = (Set[E], Set[E])

  def apply[E](replicaID: String): DeltaCRDT[State[E]] =
    DeltaCRDT.empty[State[E]](replicaID)

  def elements[E]: DeltaQuery[State[E], Set[E]] = {
    case (add, remove) => add diff remove
  }

  def insert[E](element: E): DeltaMutator[State[E]] = (_, _) =>
    (Set(element), Set.empty[E])

  def remove[E](element: E): DeltaMutator[State[E]] = (_, _) =>
    (Set.empty[E], Set(element))
}

class TwoPSet[E](crdt: DeltaCRDT[TwoPSetCRDT.State[E]]) {
  def elements: Set[E] = crdt.query(TwoPSetCRDT.elements)

  def insert(element: E): TwoPSet[E] = new TwoPSet(crdt.mutate(TwoPSetCRDT.insert(element)))

  def remove(element: E): TwoPSet[E] = new TwoPSet(crdt.mutate(TwoPSetCRDT.remove(element)))
}

object TwoPSet {
  def apply[E](replicaID: String): TwoPSet[E] = new TwoPSet(TwoPSetCRDT[E](replicaID))
}
