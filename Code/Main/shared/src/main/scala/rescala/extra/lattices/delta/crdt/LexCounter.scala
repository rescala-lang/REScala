package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.UIJDLattice._
import rescala.extra.lattices.delta._

object LexCounterCRDT {
  type State = Map[String, LexPair[Int, Int]]

  def apply(antiEntropy: AntiEntropy[State]): DeltaCRDT[State] =
    DeltaCRDT.empty[State](antiEntropy)

  def value: DeltaQuery[State, Int] = state => state.values.map(_.snd).sum

  def inc: DeltaMutator[State] = (replicaID, state) =>
    state.updatedWith(replicaID) {
      case None                => Some(LexPair(0, 1))
      case Some(LexPair(l, r)) => Some(LexPair(l, r + 1))
    }

  def dec: DeltaMutator[State] = (replicaID, state) =>
    state.updatedWith(replicaID) {
      case None                => Some(LexPair(1, -1))
      case Some(LexPair(l, r)) => Some(LexPair(l + 1, r - 1))
    }
}

class LexCounter(crdt: DeltaCRDT[LexCounterCRDT.State]) {
  def value: Int = crdt.query(LexCounterCRDT.value)

  def inc(): LexCounter = new LexCounter(crdt.mutate(LexCounterCRDT.inc))

  def dec(): LexCounter = new LexCounter(crdt.mutate(LexCounterCRDT.dec))

  def processReceivedDeltas(): LexCounter = new LexCounter(crdt.processReceivedDeltas())
}

object LexCounter {
  type State = LexCounterCRDT.State

  def apply(antiEntropy: AntiEntropy[State]): LexCounter =
    new LexCounter(LexCounterCRDT(antiEntropy))

  implicit def LexCounterStateCodec: JsonValueCodec[State] = JsonCodecMaker.make
}
