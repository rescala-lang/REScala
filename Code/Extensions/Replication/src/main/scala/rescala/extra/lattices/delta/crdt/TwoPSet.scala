package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rescala.extra.lattices.delta.DeltaCRDT.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.{AntiEntropy, DeltaCRDT}

object TwoPSetCRDT {
  type State[E] = (Set[E], Set[E])

  private def deltaState[E](
      add: Set[E] = Set.empty[E],
      remove: Set[E] = Set.empty[E]
  ): State[E] = (add, remove)

  def elements[E]: DeltaQuery[State[E], Set[E]] = {
    case (add, remove) => add diff remove
  }

  def insert[E](element: E): DeltaMutator[State[E]] = (_, _) => deltaState(add = Set(element))

  def remove[E](element: E): DeltaMutator[State[E]] = (_, _) => deltaState(remove = Set(element))
}

class TwoPSet[E](crdt: DeltaCRDT[TwoPSetCRDT.State[E]]) {
  def elements: Set[E] = crdt.query(TwoPSetCRDT.elements)

  def insert(element: E): TwoPSet[E] = new TwoPSet(crdt.mutate(TwoPSetCRDT.insert(element)))

  def remove(element: E): TwoPSet[E] = new TwoPSet(crdt.mutate(TwoPSetCRDT.remove(element)))

  def processReceivedDeltas(): TwoPSet[E] = new TwoPSet(crdt.processReceivedDeltas())
}

object TwoPSet {
  type State[E] = TwoPSetCRDT.State[E]

  def apply[E](antiEntropy: AntiEntropy[State[E]]): TwoPSet[E] =
    new TwoPSet(DeltaCRDT.empty(antiEntropy))

  implicit def TwoPSetStateCodec[E: JsonValueCodec]: JsonValueCodec[(Set[E], Set[E])] = JsonCodecMaker.make
}
