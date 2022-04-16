package kofre.decompose.interfaces

import kofre.Defs.Id
import kofre.decompose.interfaces.GCounterInterface.{GCounter, asGcounter}
import kofre.decompose.{CRDTInterface, UIJDLattice}
import kofre.syntax.{AllPermissionsCtx, DeltaMutator, DeltaQuery, FixedIdCtx, IdentifierCtx, OpsSyntaxHelper, QueryCtx}
import kofre.syntax.AllPermissionsCtx.withID

/** A PNCounter (Positive-Negative Counter) is a Delta CRDT modeling a counter.
  *
  * It is composed of two grow-only counters (see [[GCounterInterface]]) to enable both increments and decrements of the counter value.
  */
object PNCounterModule {
  type PNCounter = (GCounter, GCounter)

  implicit class PNCounterSyntax[C](container: C) extends OpsSyntaxHelper[C, PNCounter](container) {
    def value(using QueryP): Int =
      val pos = current._1.asGcounter.value
      val neg = current._2.asGcounter.value
      pos - neg

    def inc()(using MutationIDP): C =
      val pos = current._1.asGcounter.inc()(using withID(replicaID))
      mutate(pos, UIJDLattice.bottom)

    def dec()(using MutationIDP): C =
      val neg = current._2.asGcounter.inc()(using withID(replicaID))
      mutate(UIJDLattice.bottom, neg)
  }
}
