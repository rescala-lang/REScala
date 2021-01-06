package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.UIJDLatticeWithBottom.PairAsUIJDLattice
import rescala.extra.lattices.delta.{DeltaCRDT, UIJDLatticeWithBottom}

object PNCounterCRDT {
  type State = (GCounterCRDT.State, GCounterCRDT.State)

  def apply(replicaID: String): DeltaCRDT[State] =
    DeltaCRDT.empty[State](replicaID)

  def value: DeltaQuery[State, Int] = {
    case (incCounter, decCounter) => GCounterCRDT.value(incCounter) - GCounterCRDT.value(decCounter)
  }

  def inc: DeltaMutator[State] = {
    case (replicaID, (incCounter, _)) =>
      (GCounterCRDT.inc(replicaID, incCounter), UIJDLatticeWithBottom[GCounterCRDT.State].bottom)
  }

  def dec: DeltaMutator[State] = {
    case (replicaID, (_, decCounter)) =>
       (UIJDLatticeWithBottom[GCounterCRDT.State].bottom, GCounterCRDT.inc(replicaID, decCounter))
  }
}

class PNCounter(crdt: DeltaCRDT[PNCounterCRDT.State]) {
  def value: Int = crdt.query(PNCounterCRDT.value)

  def inc(): PNCounter = new PNCounter(crdt.mutate(PNCounterCRDT.inc))

  def dec(): PNCounter = new PNCounter(crdt.mutate(PNCounterCRDT.dec))
}

object PNCounter {
  def apply(replicaID: String): PNCounter = new PNCounter(PNCounterCRDT(replicaID))
}
