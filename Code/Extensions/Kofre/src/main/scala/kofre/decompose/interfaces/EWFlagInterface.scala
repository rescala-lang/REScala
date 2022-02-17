package kofre.decompose.interfaces

import kofre.decompose.*
import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose.DotStore.*
import kofre.causality.CContext

object EWFlagInterface {
  type State[C] = Causal[DotSet, C]

  trait EWFlagCompanion {
    type State[C] = EWFlagInterface.State[C]
    type Embedded = DotSet
  }

  private class DeltaStateFactory[C: CContext] {
    val bottom: State[C] = UIJDLattice[State[C]].bottom

    def make(
        ds: DotSet = bottom.dotStore,
        cc: C = bottom.cc
    ): State[C] = Causal(ds, cc)
  }

  private def deltaState[C: CContext]: DeltaStateFactory[C] = new DeltaStateFactory[C]

  def read[C: CContext]: DeltaQuery[State[C], Boolean] = {
    case Causal(ds, _) => ds.nonEmpty
  }

  def enable[C: CContext](): DeltaMutator[State[C]] = {
    case (replicaID, Causal(ds, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      deltaState[C].make(
        ds = Set(nextDot),
        cc = CContext[C].fromSet(ds + nextDot)
      )
  }

  def disable[C: CContext](): DeltaMutator[State[C]] = {
    case (_, Causal(ds, _)) =>
      deltaState[C].make(
        cc = CContext[C].fromSet(ds)
      )
  }
}

/** An EWFlag (Enable-Wins Flag) is a Delta CRDT modeling a boolean flag.
  *
  * When the flag is concurrently disabled and enabled then the enable operation wins, i.e. the resulting flag is enabled.
  */
abstract class EWFlagInterface[C: CContext, Wrapper] extends CRDTInterface[EWFlagInterface.State[C], Wrapper] {
  def read: Boolean = query(EWFlagInterface.read)

  def enable(): Wrapper = mutate(EWFlagInterface.enable())

  def disable(): Wrapper = mutate(EWFlagInterface.disable())
}
