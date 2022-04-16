package kofre.decompose.interfaces

import kofre.causality.CausalContext
import kofre.decompose.*
import kofre.syntax.{DeltaMutator, DeltaQuery}
import kofre.decompose.DotStore.*
import kofre.dotbased.CausalStore

object EWFlagInterface {
  type State = CausalStore[DotSet]

  trait EWFlagCompanion {
    type State    = EWFlagInterface.State
    type Embedded = DotSet
  }

  private class DeltaStateFactory {
    val bottom: State = UIJDLattice[State].bottom

    def make(
        ds: DotSet = bottom.store,
        cc: CausalContext = bottom.context
    ): State = CausalStore(ds, cc)
  }

  private def deltaState: DeltaStateFactory = new DeltaStateFactory

  def read: DeltaQuery[State, Boolean] = {
    case CausalStore(ds, _) => ds.nonEmpty
  }

  def enable(): DeltaMutator[State] = {
    case (replicaID, CausalStore(ds, cc)) =>
      val nextDot = cc.nextDot(replicaID)

      deltaState.make(
        ds = Set(nextDot),
        cc = CausalContext.fromSet(ds + nextDot)
      )
  }

  def disable(): DeltaMutator[State] = {
    case (_, CausalStore(ds, _)) =>
      deltaState.make(
        cc = CausalContext.fromSet(ds)
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
