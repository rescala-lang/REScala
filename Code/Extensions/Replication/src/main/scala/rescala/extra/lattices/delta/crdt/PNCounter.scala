package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.{AntiEntropy, DeltaCRDT, UIJDLattice}

object PNCounterCRDT {
  type State = (GCounterCRDT.State, GCounterCRDT.State)

  def apply(antiEntropy: AntiEntropy[State]): DeltaCRDT[State] =
    DeltaCRDT.empty[State](antiEntropy)

  def value: DeltaQuery[State, Int] = {
    case (incCounter, decCounter) => GCounterCRDT.value(incCounter) - GCounterCRDT.value(decCounter)
  }

  def inc: DeltaMutator[State] = {
    case (replicaID, (incCounter, _)) =>
      (GCounterCRDT.inc(replicaID, incCounter), UIJDLattice[GCounterCRDT.State].bottom)
  }

  def dec: DeltaMutator[State] = {
    case (replicaID, (_, decCounter)) =>
      (UIJDLattice[GCounterCRDT.State].bottom, GCounterCRDT.inc(replicaID, decCounter))
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
    new PNCounter(PNCounterCRDT(antiEntropy))

  implicit def PNCounterStateCodec: JsonValueCodec[(Map[String, Int], Map[String, Int])] = JsonCodecMaker.make
}
