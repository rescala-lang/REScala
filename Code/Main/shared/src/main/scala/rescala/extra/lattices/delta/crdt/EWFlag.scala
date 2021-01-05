package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta._

object EWFlag {
  type State[C] = Causal[DotSet, C]

  def apply[C: CContext](replicaID: String): DeltaCRDT[State[C]] =
    DeltaCRDT(replicaID, UIJDLatticeWithBottom[State[C]].bottom, List())

  def read[C: CContext]: DeltaQuery[State[C], Boolean] = {
    case Causal(ds, _) => ds.nonEmpty
  }

  def enable[C: CContext]: DeltaMutator[State[C]] = {
    case (replicaID, Causal(ds, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      Delta(
        replicaID,
        Causal(
          Set(nextDot),
          CContext[C].fromSet(ds + nextDot)
        )
      )
  }

  def disable[C: CContext]: DeltaMutator[State[C]] = {
    case (replicaID, Causal(ds, _)) =>
      Delta(
        replicaID,
        Causal(
          DotSet.empty,
          CContext[C].fromSet(ds)
        )
      )
  }
}
