package kofre.decompose.interfaces

import kofre.causality.{CContext, Causal, CausalContext}
import kofre.decompose.*
import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose.DotStore.*

object EWFlagInterface {
  type State = Causal[DotSet]

  trait EWFlagCompanion {
    type State = EWFlagInterface.State
    type Embedded = DotSet
  }

  private class DeltaStateFactory {
    val bottom: State = UIJDLattice[State].bottom

    def make(
        ds: DotSet = bottom.dotStore,
        cc: CausalContext = bottom.cc
    ): State = Causal(ds, cc)
  }

  private def deltaState: DeltaStateFactory = new DeltaStateFactory

  def read: DeltaQuery[State, Boolean] = {
    case Causal(ds, _) => ds.nonEmpty
  }

  def enable(): DeltaMutator[State] = {
    case (replicaID, Causal(ds, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      deltaState.make(
        ds = Set(nextDot),
        cc = CContext[C].fromSet(ds + nextDot)
      )
  }

  def disable(): DeltaMutator[State] = {
    case (_, Causal(ds, _)) =>
      deltaState.make(
        cc = CContext[C].fromSet(ds)
      )
  }
}

/** An EWFlag (Enable-Wins Flag) is a Delta CRDT modeling a boolean flag.
  *
  * When the flag is concurrently disabled and enabled then the enable operation wins, i.e. the resulting flag is enabled.
  */
abstract class EWFlagInterface[Wrapper] extends CRDTInterface[EWFlagInterface.State, Wrapper] {
  def read: Boolean = query(EWFlagInterface.read)

  def enable(): Wrapper = mutate(EWFlagInterface.enable())

  def disable(): Wrapper = mutate(EWFlagInterface.disable())
}
