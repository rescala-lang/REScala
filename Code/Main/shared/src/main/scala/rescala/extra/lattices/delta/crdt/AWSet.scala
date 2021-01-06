package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta.{CContext, Causal, DeltaCRDT}

object AWSetCRDT {
  type State[E, C] = Causal[DotMap[E, DotSet], C]

  def apply[E, C: CContext](replicaID: String): DeltaCRDT[State[E, C]] =
    DeltaCRDT.empty[State[E, C]](replicaID)

  def elements[E, C: CContext]: DeltaQuery[State[E, C], Set[E]] = {
    case Causal(dm, _) => dm.keySet
  }

  def add[E, C: CContext](e: E): DeltaMutator[State[E, C]] = {
    case (replicaID, Causal(dm, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      Causal(
        DotMap[E, DotSet].empty.updated(e, Set(nextDot)),
        CContext[C].fromSet(dm(e) + nextDot)
      )
  }

  def remove[E, C: CContext](e: E): DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      Causal(
        DotMap[E, DotSet].empty,
        CContext[C].fromSet(dm(e))
      )
  }

  def clear[E, C: CContext]: DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      Causal(
        DotMap[E, DotSet].empty,
        CContext[C].fromSet(DotMap[E, DotSet].dots(dm))
      )
  }
}

class AWSet[E, C: CContext](crdt: DeltaCRDT[AWSetCRDT.State[E, C]]) {
  def elements: Set[E] = crdt.query(AWSetCRDT.elements)

  def add(e: E): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.add(e)))

  def remove(e: E): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.remove(e)))

  def clear(): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.clear))
}

object AWSet {
  def apply[E, C: CContext](replicaID: String): AWSet[E, C] = new AWSet(AWSetCRDT[E, C](replicaID))
}
