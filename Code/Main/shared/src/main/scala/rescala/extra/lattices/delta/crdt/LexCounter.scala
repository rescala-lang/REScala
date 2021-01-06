package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.UIJDLatticeWithBottom.MapAsUIJDLattice
import rescala.extra.lattices.delta.{DeltaCRDT, LexPair}

object LexCounter {
  type State = Map[String, LexPair[Int, Int]]

  def apply(replicaID: String): DeltaCRDT[State] =
    DeltaCRDT.empty[State](replicaID)

  def value: DeltaQuery[State, Int] = state =>
    state.values.map(_.snd).sum

  def inc: DeltaMutator[State] = (replicaID, state) =>
    state.updatedWith(replicaID) {
      case None => Some(LexPair(0, 1))
      case Some(LexPair(l, r)) => Some(LexPair(l, r + 1))
    }

  def dec: DeltaMutator[State] = (replicaID, state) =>
    state.updatedWith(replicaID) {
      case None => Some(LexPair(1, -1))
      case Some(LexPair(l, r)) => Some(LexPair(l + 1, r - 1))
    }
}
