package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta._

object ORMapCRDT {
  type State[K, V, C] = Causal[DotMap[K, V], C]

  def apply[K, V: DotStore, C: CContext](antiEntropy: AntiEntropy[State[K, V, C]]): DeltaCRDT[State[K, V, C]] =
    DeltaCRDT.empty[State[K, V, C]](antiEntropy)

  def queryKey[K, V: DotStore, A, C: CContext](k: K, q: DeltaQuery[Causal[V, C], A]): DeltaQuery[State[K, V, C], A] = {
    case Causal(dm, cc) =>
      val v = dm.getOrElse(k, DotStore[V].empty)
      q(Causal(v, cc))
  }

  def mutateKey[K, V: DotStore, C: CContext](k: K, m: DeltaMutator[Causal[V, C]]): DeltaMutator[State[K, V, C]] = {
    case (replicaID, Causal(dm, cc)) =>
      val v = dm.getOrElse(k, DotStore[V].empty)

      m(replicaID, Causal(v, cc)) match {
      case Causal(stateDelta, ccDelta) =>
        Causal(
          DotMap[K, V].empty.updated(k, stateDelta),
          ccDelta
        )
    }
  }

  def remove[K, V: DotStore, C: CContext](k: K): DeltaMutator[State[K, V, C]] = {
    case (_, Causal(dm, _)) =>
      val v = dm.getOrElse(k, DotStore[V].empty)

      Causal(
        DotMap[K, V].empty,
        CContext[C].fromSet(DotStore[V].dots(v))
      )
  }

  def clear[K, V: DotStore, C: CContext]: DeltaMutator[State[K, V, C]] = {
    case (_, Causal(dm, _)) =>
      Causal(
        DotMap[K, V].empty,
        CContext[C].fromSet(DotMap[K, V].dots(dm))
      )
  }
}

class ORMap[K, V: DotStore, C: CContext](crdt: DeltaCRDT[ORMapCRDT.State[K, V, C]]) {
  def queryKey[A](k: K, q: DeltaQuery[Causal[V, C], A]): A = crdt.query(ORMapCRDT.queryKey(k, q))

  def mutateKey(k: K, m: DeltaMutator[Causal[V, C]]): ORMap[K, V, C] = new ORMap(crdt.mutate(ORMapCRDT.mutateKey(k, m)))

  def remove(k: K): ORMap[K, V, C] = new ORMap(crdt.mutate(ORMapCRDT.remove(k)))

  def clear(): ORMap[K, V, C] = new ORMap(crdt.mutate(ORMapCRDT.clear))

  def processReceivedDeltas(): ORMap[K, V, C] = new ORMap(crdt.processReceivedDeltas())
}

object ORMap {
  type State[K, V, C] = ORMapCRDT.State[K, V, C]
  type Embedded[K, V] = DotMap[K, V]

  def apply[K, V: DotStore, C: CContext](antiEntropy: AntiEntropy[State[K, V, C]]): ORMap[K, V, C] =
    new ORMap(ORMapCRDT[K, V, C](antiEntropy))

  implicit def ORMapStateCodec[K: JsonValueCodec, V: JsonValueCodec, C: JsonValueCodec]: JsonValueCodec[Causal[Map[K, V], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def ORMapEmbeddedCodec[K: JsonValueCodec, V: JsonValueCodec]: JsonValueCodec[Map[K, V]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}
