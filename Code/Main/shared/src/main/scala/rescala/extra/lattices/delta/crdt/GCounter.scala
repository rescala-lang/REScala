package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.UIJDLatticeWithBottom._

object GCounterCRDT {
  type State = Map[String, Int]

  def apply(replicaID: String): DeltaCRDT[State] =
    DeltaCRDT.empty[State](replicaID)

  def value: DeltaQuery[State, Int] = state => state.values.sum

  def inc: DeltaMutator[State] = (replicaID, state) =>
      Map(replicaID -> (state.getOrElse(replicaID, 0) + 1))
}

class GCounter(crdt: DeltaCRDT[GCounterCRDT.State]) {
  def value: Int = crdt.query(GCounterCRDT.value)

  def inc(): GCounter = new GCounter(crdt.mutate(GCounterCRDT.inc))
}

object GCounter {
  def apply(replicaID: String): GCounter = new GCounter(GCounterCRDT(replicaID))
}
