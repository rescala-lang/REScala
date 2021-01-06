package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.UIJDLatticeWithBottom.PairAsUIJDLattice
import rescala.extra.lattices.delta.{DeltaCRDT, UIJDLatticeWithBottom}

object PNCounter {
  type State = (GCounter.State, GCounter.State)

  def apply(replicaID: String): DeltaCRDT[State] =
    DeltaCRDT.empty[State](replicaID)

  def value: DeltaQuery[State, Int] = {
    case (incCounter, decCounter) => GCounter.value(incCounter) - GCounter.value(decCounter)
  }

  def inc: DeltaMutator[State] = {
    case (replicaID, (incCounter, _)) =>
      (GCounter.inc(replicaID, incCounter), UIJDLatticeWithBottom[GCounter.State].bottom)
  }

  def dec: DeltaMutator[State] = {
    case (replicaID, (_, decCounter)) =>
       (UIJDLatticeWithBottom[GCounter.State].bottom, GCounter.inc(replicaID, decCounter))
  }
}
