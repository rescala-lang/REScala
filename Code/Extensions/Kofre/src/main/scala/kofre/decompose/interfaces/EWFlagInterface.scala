package kofre.decompose.interfaces

import kofre.causality.CausalContext
import kofre.decompose.*
import kofre.syntax.{DeltaMutator, DeltaQuery, OpsSyntaxHelper}
import kofre.decompose.DotStore.*
import kofre.dotbased.CausalStore
import kofre.primitives.Epoche

object EWFlagInterface {
  type EWFlag = CausalStore[DotSet]

  implicit class EWFlagSyntax[C](container: C) extends OpsSyntaxHelper[C, EWFlag](container) {
    def read(using QueryP): Boolean = current.store.nonEmpty

    def enable()(using MutationIDP): C = {
      val nextDot = current.context.nextDot(replicaID)
      CausalStore(
        Set(nextDot),
        CausalContext.fromSet(current.store + nextDot)
      )
    }
    def disable()(using MutationP): C = {
      CausalStore(
        DotSet.empty,
        CausalContext.fromSet(current.store)
      )
    }
  }

}
