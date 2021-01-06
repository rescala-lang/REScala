package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.{Delta, DeltaCRDT}
import rescala.extra.lattices.delta.DeltaCRDT.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.UIJDLatticeWithBottom.SetAsUIJDLattice

object GSet {
  type State[E] = Set[E]

  def apply[E](replicaID: String): DeltaCRDT[State[E]] =
    DeltaCRDT.empty[Set[E]](replicaID)

  def elements[E]: DeltaQuery[State[E], Set[E]] = state => state

  def insert[E](element: E): DeltaMutator[State[E]] = (replicaID, _) =>
    Delta(
      replicaID,
      Set(element)
    )
}
