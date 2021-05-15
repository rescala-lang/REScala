package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rescala.extra.lattices.delta.DeltaCRDT.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.{
  AntiEntropy, CContext, Causal, Delta, DeltaCRDT, Dot, RDeltaCRDT, TimedVal, UIJDLattice
}

object LastWriterWinsCRDT {
  type State[A, C] = MVRegister.State[TimedVal[A], C]

  def read[A, C: CContext]: DeltaQuery[State[A, C], Option[A]] =
    MVRegisterCRDT.read[TimedVal[A], C].andThen(s => s.reduceOption(UIJDLattice[TimedVal[A]].merge).map(_.value))

  def write[A, C: CContext](v: A): DeltaMutator[State[A, C]] =
    (replicaID, state) => MVRegisterCRDT.write(TimedVal(v, replicaID)).apply(replicaID, state)

  def clear[A, C: CContext](): DeltaMutator[State[A, C]] = MVRegisterCRDT.clear
}

class LastWriterWins[A, C: CContext](crdt: DeltaCRDT[LastWriterWins.State[A, C]]) {
  def read: Option[A] = crdt.query(LastWriterWinsCRDT.read)

  def write(v: A): LastWriterWins[A, C] = new LastWriterWins(crdt.mutate(LastWriterWinsCRDT.write(v)))

  def clear(): LastWriterWins[A, C] = new LastWriterWins(crdt.mutate(LastWriterWinsCRDT.clear()))

  def processReceivedDeltas(): LastWriterWins[A, C] = new LastWriterWins(crdt.processReceivedDeltas())
}

object LastWriterWins {
  type State[A, C] = LastWriterWinsCRDT.State[A, C]
  type Embedded[A] = DotFun[TimedVal[A]]

  def apply[A, C: CContext](ae: AntiEntropy[State[A, C]]): LastWriterWins[A, C] =
    new LastWriterWins(DeltaCRDT.empty[State[A, C]](ae))

  implicit def LastWriterWinsStateCodec[A: JsonValueCodec, C: JsonValueCodec]
      : JsonValueCodec[Causal[Map[Dot, TimedVal[A]], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def LastWriterWinsEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[Map[Dot, TimedVal[A]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}

class RLastWriterWins[A, C: CContext](val crdt: RDeltaCRDT[RLastWriterWins.State[A, C]])
    extends CRDTInterface[RLastWriterWins.State[A, C]] {
  def read: Option[A] = crdt.query(LastWriterWinsCRDT.read)

  def write(v: A): RLastWriterWins[A, C] = new RLastWriterWins(crdt.mutate(LastWriterWinsCRDT.write(v)))

  def clear(): RLastWriterWins[A, C] = new RLastWriterWins(crdt.mutate(LastWriterWinsCRDT.clear()))

  def applyDelta(delta: Delta[RLastWriterWins.State[A, C]]): RLastWriterWins[A, C] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this else new RLastWriterWins(newCRDT)
  }
}

object RLastWriterWins {
  type State[A, C] = LastWriterWinsCRDT.State[A, C]
  type Embedded[A] = DotFun[TimedVal[A]]

  def apply[A, C: CContext](replicaID: String): RLastWriterWins[A, C] =
    new RLastWriterWins(RDeltaCRDT.empty[State[A, C]](replicaID))

  implicit def LastWriterWinsStateCodec[A: JsonValueCodec, C: JsonValueCodec]
      : JsonValueCodec[Causal[Map[Dot, TimedVal[A]], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def LastWriterWinsEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[Map[Dot, TimedVal[A]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}
