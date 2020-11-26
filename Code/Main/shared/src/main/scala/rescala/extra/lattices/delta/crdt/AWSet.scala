package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta.SetDelta

object AWSet {
  type Store[E] = DotMap[E, DotSet]

  def elements[E]: DeltaQuery[Store[E], Set[E]] = dm => dm.keySet

  def add[E](e: E): DeltaDotMutator[Store[E]] = (dm, nextDot) =>
    SetDelta(Map(e -> Set(nextDot)), dm.getOrElse(e, DotSet.bottom) + nextDot)

  def remove[E](e: E): DeltaMutator[Store[E]] = dm =>
    SetDelta(DotMap[E, DotSet].bottom, dm.getOrElse(e, DotSet.bottom))

  def clear[E]: DeltaMutator[Store[E]] = dm =>
    SetDelta(DotMap[E, DotSet].bottom, dots(dm))
}
