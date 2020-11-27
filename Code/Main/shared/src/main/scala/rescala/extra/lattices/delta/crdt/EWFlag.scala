package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore.DotSet
import rescala.extra.lattices.delta.{CContext, DeltaCRDT, SetDelta}

object EWFlag {
  def apply[C: CContext](replicaID: String): DeltaCRDT[DotSet, C] = DeltaCRDT(replicaID, DotSet.bottom, CContext[C].empty, List())

  def read: DeltaQuery[DotSet, Boolean] = ds => ds.nonEmpty

  def enable: DeltaDotMutator[DotSet] = (ds, nextDot) => SetDelta(Set(nextDot), ds + nextDot)

  def disable: DeltaMutator[DotSet] = ds => SetDelta(DotSet.bottom, ds)
}
