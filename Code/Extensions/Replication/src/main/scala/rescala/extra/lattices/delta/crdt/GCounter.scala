package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.UIJDLattice._
import rescala.extra.lattices.delta._

object GCounterCRDT {
  type State = Map[String, Int]

  implicit val StateUIJDLattice: UIJDLattice[State] = MapAsUIJDLattice[String, Int]

  def value: DeltaQuery[State, Int] = state => state.values.sum

  def inc(): DeltaMutator[State] = (replicaID, state) => Map(replicaID -> (state.getOrElse(replicaID, 0) + 1))
}

class GCounter(crdt: DeltaCRDT[GCounterCRDT.State]) {
  def value: Int = crdt.query(GCounterCRDT.value)

  def inc(): GCounter = new GCounter(crdt.mutate(GCounterCRDT.inc()))

  def processReceivedDeltas(): GCounter = new GCounter(crdt.processReceivedDeltas())
}

object GCounter {
  type State = GCounterCRDT.State

  def apply(antiEntropy: AntiEntropy[State]): GCounter =
    new GCounter(DeltaCRDT.empty(antiEntropy))
}
