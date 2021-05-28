package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.{AntiEntropy, DeltaCRDT}

object GSetCRDT {
  type State[E] = Set[E]

  def elements[E]: DeltaQuery[State[E], Set[E]] = state => state

  def insert[E](element: E): DeltaMutator[State[E]] = (_, _) => Set(element)
}

class GSet[E](crdt: DeltaCRDT[GSetCRDT.State[E]]) {
  def elements: Set[E] = crdt.query(GSetCRDT.elements)

  def insert(element: E): GSet[E] = new GSet(crdt.mutate(GSetCRDT.insert(element)))

  def processReceivedDeltas(): GSet[E] = new GSet(crdt.processReceivedDeltas())
}

object GSet {
  type State[E] = GSetCRDT.State[E]

  def apply[E](antiEntropy: AntiEntropy[State[E]]): GSet[E] =
    new GSet(DeltaCRDT.empty(antiEntropy))

  implicit def GSetStateCodec[E: JsonValueCodec]: JsonValueCodec[Set[E]] = JsonCodecMaker.make
}
