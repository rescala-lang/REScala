package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.{AntiEntropy, DeltaCRDT, UIJDLattice}

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

class PNCounter(crdt: DeltaCRDT[PNCounterCRDT.State]) {
  def value: Int = crdt.query(PNCounterCRDT.value)

  def inc(): PNCounter = new PNCounter(crdt.mutate(PNCounterCRDT.inc))

  def dec(): PNCounter = new PNCounter(crdt.mutate(PNCounterCRDT.dec))

  def processReceivedDeltas(): PNCounter = new PNCounter(crdt.processReceivedDeltas())
}

object PNCounter {
  type State = PNCounterCRDT.State

  def apply(antiEntropy: AntiEntropy[PNCounterCRDT.State]): PNCounter =
    new PNCounter(DeltaCRDT.empty(antiEntropy))
}
