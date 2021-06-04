package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta._

object PNCounterCRDT {
  type State = (GCounterCRDT.State, GCounterCRDT.State)

  private def deltaState(
      pos: GCounterCRDT.State = UIJDLattice[GCounterCRDT.State].bottom,
      neg: GCounterCRDT.State = UIJDLattice[GCounterCRDT.State].bottom
  ): State = (pos, neg)

  def value: DeltaQuery[State, Int] = {
    case (incCounter, decCounter) => GCounterCRDT.value(incCounter) - GCounterCRDT.value(decCounter)
  }

  def inc: DeltaMutator[State] = {
    case (replicaID, (incCounter, _)) =>
      deltaState(pos = GCounterCRDT.inc()(replicaID, incCounter))
  }

  def dec: DeltaMutator[State] = {
    case (replicaID, (_, decCounter)) =>
      deltaState(neg = GCounterCRDT.inc()(replicaID, decCounter))
  }
}
