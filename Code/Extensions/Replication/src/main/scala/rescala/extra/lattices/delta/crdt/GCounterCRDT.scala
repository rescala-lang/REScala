package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.UIJDLattice._
import rescala.extra.lattices.delta._

object GCounterCRDT {
  type State = Map[String, Int]

  implicit val StateUIJDLattice: UIJDLattice[State] = MapAsUIJDLattice[String, Int]

  def value: DeltaQuery[State, Int] = state => state.values.sum

  def inc(): DeltaMutator[State] = (replicaID, state) => Map(replicaID -> (state.getOrElse(replicaID, 0) + 1))
}
