package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta._
import rescala.extra.lattices.delta.crdt.ORMapCRDT.State

object ORMapCRDT {
  type State[K, V, C] = Causal[DotMap[K, V], C]

  def apply[K, V: DotStore, C: CContext](antiEntropy: AntiEntropy[State[K, V, C]]): DeltaCRDT[State[K, V, C]] =
    DeltaCRDT.empty[State[K, V, C]](antiEntropy)

  def contains[K, V: DotStore, C: CContext](k: K): DeltaQuery[State[K, V, C], Boolean] = {
    case Causal(dm, _) => dm.contains(k)
  }

  def queryKey[K, V: DotStore, A, C: CContext](k: K, q: DeltaQuery[Causal[V, C], A]): DeltaQuery[State[K, V, C], A] = {
    case Causal(dm, cc) =>
      val v = dm.getOrElse(k, DotStore[V].empty)
      q(Causal(v, cc))
  }

  def queryAllEntries[K, V: DotStore, A, C: CContext](q: DeltaQuery[Causal[V, C], A])
      : DeltaQuery[State[K, V, C], Iterable[A]] = {
    case Causal(dm, cc) =>
      dm.values.map(v => q(Causal(v, cc)))
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

  def removeAll[K, V: DotStore, C: CContext](keys: Iterable[K]): DeltaMutator[State[K, V, C]] = {
    case (_, Causal(dm, _)) =>
      val values = keys.map(k => dm.getOrElse(k, DotStore[V].empty))
      val dots = values.foldLeft(Set.empty[Dot]) {
        case (set, v) => set union DotStore[V].dots(v)
      }

      Causal(
        DotMap[K, V].empty,
        CContext[C].fromSet(dots)
      )
  }

  def removeByValue[K, V: DotStore, C: CContext](cond: Causal[V, C] => Boolean): DeltaMutator[State[K, V, C]] = {
    case (_, Causal(dm, cc)) =>
      val toRemove = dm.values.collect {
        case v if cond(Causal(v, cc)) => DotStore[V].dots(v)
      }.fold(Set())(_ union _)

      Causal(
        DotMap[K, V].empty,
        CContext[C].fromSet(toRemove)
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

  def contains(k: K): Boolean = crdt.query(ORMapCRDT.contains(k))

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

  implicit def ORMapStateCodec[K: JsonValueCodec, V: JsonValueCodec, C: JsonValueCodec]
      : JsonValueCodec[Causal[Map[K, V], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def ORMapEmbeddedCodec[K: JsonValueCodec, V: JsonValueCodec]: JsonValueCodec[Map[K, V]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}

class RORMap[K, V: DotStore, C: CContext](val crdt: RDeltaCRDT[ORMapCRDT.State[K, V, C]])
    extends CRDTInterface[ORMapCRDT.State[K, V, C]] {
  def queryKey[A](k: K, q: DeltaQuery[Causal[V, C], A]): A = crdt.query(ORMapCRDT.queryKey(k, q))

  def contains(k: K): Boolean = crdt.query(ORMapCRDT.contains(k))

  def queryAllEntries[A](q: DeltaQuery[Causal[V, C], A]): Iterable[A] = crdt.query(ORMapCRDT.queryAllEntries(q))

  def mutateKey(k: K, m: DeltaMutator[Causal[V, C]]): RORMap[K, V, C] =
    new RORMap(crdt.mutate(ORMapCRDT.mutateKey(k, m)))

  def remove(k: K): RORMap[K, V, C] = new RORMap(crdt.mutate(ORMapCRDT.remove(k)))

  def removeAll(keys: Iterable[K]): RORMap[K, V, C] = new RORMap(crdt.mutate(ORMapCRDT.removeAll(keys)))

  def removeByValue(cond: Causal[V, C] => Boolean): RORMap[K, V, C] =
    new RORMap(crdt.mutate(ORMapCRDT.removeByValue(cond)))

  def clear(): RORMap[K, V, C] = new RORMap(crdt.mutate(ORMapCRDT.clear))

  def applyDelta(delta: Delta[State[K, V, C]]): RORMap[K, V, C] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this else new RORMap(newCRDT)
  }
}

object RORMap {
  type State[K, V, C] = ORMapCRDT.State[K, V, C]
  type Embedded[K, V] = DotMap[K, V]

  def apply[K, V: DotStore, C: CContext](replicaID: String): RORMap[K, V, C] =
    new RORMap(RDeltaCRDT.empty[State[K, V, C]](replicaID))

  implicit def ORMapStateCodec[K: JsonValueCodec, V: JsonValueCodec, C: JsonValueCodec]
      : JsonValueCodec[Causal[Map[K, V], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def ORMapEmbeddedCodec[K: JsonValueCodec, V: JsonValueCodec]: JsonValueCodec[Map[K, V]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}
