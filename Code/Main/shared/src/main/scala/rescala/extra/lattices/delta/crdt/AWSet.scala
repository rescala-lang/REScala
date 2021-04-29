package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta._

object AWSetCRDT {
  type State[E, C] = Causal[DotMap[E, DotSet], C]

  def apply[E, C: CContext](antiEntropy: AntiEntropy[State[E, C]]): DeltaCRDT[State[E, C]] =
    DeltaCRDT.empty[State[E, C]](antiEntropy)

  def elements[E, C: CContext]: DeltaQuery[State[E, C], Set[E]] = {
    case Causal(dm, _) => dm.keySet
  }

  def add[E, C: CContext](e: E): DeltaMutator[State[E, C]] = {
    case (replicaID, Causal(dm, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)
      val v       = dm.getOrElse(e, DotSet.empty)

      Causal(
        DotMap[E, DotSet].empty.updated(e, Set(nextDot)),
        CContext[C].fromSet(v + nextDot)
      )
  }

  def remove[E, C: CContext](e: E): DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      val v = dm.getOrElse(e, DotSet.empty)

      Causal(
        DotMap[E, DotSet].empty,
        CContext[C].fromSet(v)
      )
  }

  def removeBy[E, C: CContext](cond: E => Boolean): DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      val removedDots = dm.collect {
        case (k, v) if cond(k) => v
      }.foldLeft(DotSet.empty)(_ union _)

      Causal(
        DotMap[E, DotSet].empty,
        CContext[C].fromSet(removedDots)
      )
  }

  def clear[E, C: CContext]: DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      Causal(
        DotMap[E, DotSet].empty,
        CContext[C].fromSet(DotMap[E, DotSet].dots(dm))
      )
  }
}

class AWSet[E, C: CContext](crdt: DeltaCRDT[AWSetCRDT.State[E, C]]) {
  def elements: Set[E] = crdt.query(AWSetCRDT.elements)

  def add(e: E): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.add(e)))

  def remove(e: E): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.remove(e)))

  def removeBy(cond: E => Boolean): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.removeBy(cond)))

  def clear(): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.clear))

  def processReceivedDeltas(): AWSet[E, C] = new AWSet(crdt.processReceivedDeltas())
}

object AWSet {
  type State[E, C] = AWSetCRDT.State[E, C]
  type Embedded[E] = DotMap[E, DotSet]

  def apply[E, C: CContext](antiEntropy: AntiEntropy[State[E, C]]): AWSet[E, C] =
    new AWSet(AWSetCRDT[E, C](antiEntropy))

  implicit def AWSetStateCodec[E: JsonValueCodec, C: JsonValueCodec]: JsonValueCodec[Causal[Map[E, Set[Dot]], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def AWSetEmbeddedCodec[E: JsonValueCodec]: JsonValueCodec[Map[E, Set[Dot]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}

abstract class CRDTInterface[A: UIJDLattice] {
  val crdt: RDeltaCRDT[A]

  def applyDelta(delta: Delta[A]): CRDTInterface[A]

  def lastDelta: Option[Delta[A]] = crdt.lastDelta
}

class RAWSet[E, C: CContext](val crdt: RDeltaCRDT[AWSetCRDT.State[E, C]]) extends CRDTInterface[AWSetCRDT.State[E, C]] {
  def elements: Set[E] = crdt.query(AWSetCRDT.elements)

  def add(e: E): RAWSet[E, C] = new RAWSet(crdt.mutate(AWSetCRDT.add(e)))

  def remove(e: E): RAWSet[E, C] = new RAWSet(crdt.mutate(AWSetCRDT.remove(e)))

  def removeBy(cond: E => Boolean): RAWSet[E, C] = new RAWSet(crdt.mutate(AWSetCRDT.removeBy(cond)))

  def clear(): RAWSet[E, C] = new RAWSet(crdt.mutate(AWSetCRDT.clear))

  def applyDelta(delta: Delta[AWSetCRDT.State[E, C]]): RAWSet[E, C] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this else new RAWSet(newCRDT)
  }
}

object RAWSet {
  type State[E, C] = AWSetCRDT.State[E, C]
  type Embedded[E] = DotMap[E, DotSet]

  def apply[E, C: CContext](replicaID: String): RAWSet[E, C] =
    new RAWSet(RDeltaCRDT.empty[State[E, C]](replicaID))

  implicit def AWSetStateCodec[E: JsonValueCodec, C: JsonValueCodec]: JsonValueCodec[Causal[Map[E, Set[Dot]], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def AWSetEmbeddedCodec[E: JsonValueCodec]: JsonValueCodec[Map[E, Set[Dot]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}
