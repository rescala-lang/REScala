package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta.{CContext, Causal, Delta, DeltaCRDT, UIJDLatticeWithBottom}

object AWSet {
  type State[E, C] = Causal[DotMap[E, DotSet], C]

  def apply[E, C: CContext](replicaID: String): DeltaCRDT[State[E, C]] =
    DeltaCRDT(replicaID, UIJDLatticeWithBottom[State[E, C]].bottom, List())

  def elements[E, C: CContext]: DeltaQuery[State[E, C], Set[E]] = {
    case Causal(dm, _) => dm.keySet
  }

  def add[E, C: CContext](e: E): DeltaMutator[State[E, C]] = {
    case (replicaID, Causal(dm, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      Delta(
        replicaID,
        Causal(
          DotMap[E, DotSet].empty.updated(e, Set(nextDot)),
          CContext[C].fromSet(dm(e) + nextDot)
        )
      )
  }

  def remove[E, C: CContext](e: E): DeltaMutator[State[E, C]] = {
    case (replicaID, Causal(dm, cc)) =>
      Delta(
        replicaID,
        Causal(
          DotMap[E, DotSet].empty,
          CContext[C].fromSet(dm(e))
        )
      )
  }

  def clear[E, C: CContext](e: E): DeltaMutator[State[E, C]] = {
    case (replicaID, Causal(dm, cc)) =>
      Delta(
        replicaID,
        Causal(
          DotMap[E, DotSet].empty,
          CContext[C].fromSet(DotMap[E, DotSet].dots(dm))
        )
      )
  }
}
