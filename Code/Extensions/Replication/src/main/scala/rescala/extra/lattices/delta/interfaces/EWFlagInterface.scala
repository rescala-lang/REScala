package rescala.extra.lattices.delta.interfaces

import rescala.extra.lattices.delta.CRDTInterface.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta._

object EWFlagInterface {
  type State[C] = Causal[DotSet, C]

  trait EWFlagCompanion {
    type State[C] = EWFlagInterface.State[C]
    type Embedded = DotSet
  }

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

abstract class EWFlagInterface[C: CContext, Wrapper] extends CRDTInterface[EWFlagInterface.State[C], Wrapper] {
  def read: Boolean = query(EWFlagInterface.read)

  def enable(): Wrapper = mutate(EWFlagInterface.enable())

  def disable(): Wrapper = mutate(EWFlagInterface.disable())
}
