package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta._

object EWFlagCRDT {
  type State[C] = Causal[DotSet, C]

  private def deltaState[C: CContext](
      ds: Option[DotSet] = None,
      cc: C
  ): State[C] = {
    val bottom = UIJDLattice[State[C]].bottom

    Causal(
      ds.getOrElse(bottom.dotStore),
      cc
    )
  }

  def read[C: CContext]: DeltaQuery[State[C], Boolean] = {
    case Causal(ds, _) => ds.nonEmpty
  }

  def enable[C: CContext](): DeltaMutator[State[C]] = {
    case (replicaID, Causal(ds, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      deltaState(
        ds = Some(Set(nextDot)),
        cc = CContext[C].fromSet(ds + nextDot)
      )
  }

  def disable[C: CContext](): DeltaMutator[State[C]] = {
    case (_, Causal(ds, _)) =>
      deltaState(
        cc = CContext[C].fromSet(ds)
      )
  }
}
