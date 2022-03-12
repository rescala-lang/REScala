package kofre.decompose.interfaces

import kofre.Defs.Id
import kofre.decompose.{CRDTInterface, UIJDLattice}
import kofre.syntax.{AllPermissionsCtx, DeltaMutator, DeltaQuery, OpsSyntaxHelper, QueryCtx}

/** A PNCounter (Positive-Negative Counter) is a Delta CRDT modeling a counter.
  *
  * It is composed of two grow-only counters (see [[GCounterInterface]]) to enable both increments and decrements of the counter value.
  */
object PNCounterModule {
  type PNCounter = (GCounterInterface.State, GCounterInterface.State)

  implicit class PNCounterSyntax[C](container: C) extends OpsSyntaxHelper[C, PNCounter](container) {
    def value(using QueryP): Int =
      GCounterInterface.value(current._1) - GCounterInterface.value(current._2)

    def inc()(using MutationIDP): C =
      mutate(GCounterInterface.inc()(replicaID, current._1), UIJDLattice.bottom)

    def dec()(using MutationIDP): C =
      mutate(UIJDLattice.bottom, GCounterInterface.inc()(replicaID, current._2))
  }
}
