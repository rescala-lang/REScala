package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.{Delta, DeltaCRDT}
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.UIJDLatticeWithBottom._

object GCounter {
  type State = Map[String, Int]

  def apply(replicaID: String): DeltaCRDT[State] =
    DeltaCRDT.empty[State](replicaID)

  def value: DeltaQuery[State, Int] = state => state.values.sum

  def inc: DeltaMutator[State] = (replicaID, state) =>
    Delta(
      replicaID,
      Map(replicaID -> (state.getOrElse(replicaID, 0) + 1))
    )
}
