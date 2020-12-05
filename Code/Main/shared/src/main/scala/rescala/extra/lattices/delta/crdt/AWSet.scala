package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta.{CContext, DeltaCRDT, SetDelta}

object AWSet {
  type Store[E] = DotMap[E, DotSet]

  def apply[E, C: CContext](replicaID: String): DeltaCRDT[Store[E], C] =
    DeltaCRDT(replicaID, DotMap[E, DotSet].bottom, CContext[C].empty, List())

  def elements[E]: DeltaQuery[Store[E], Set[E]] = dm => dm.keySet

  def add[E](e: E): DeltaDotMutator[Store[E]] = (dm, nextDot) =>
    SetDelta(DotMap[E, DotSet].bottom.updated(e, Set(nextDot)), dm(e) + nextDot)

  def remove[E](e: E): DeltaMutator[Store[E]] = dm =>
    SetDelta(DotMap[E, DotSet].bottom, dm(e))

  def clear[E]: DeltaMutator[Store[E]] = dm =>
    SetDelta(DotMap[E, DotSet].bottom, DotMap[E, DotSet].dots(dm))
}
