package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.{CContext, Causal, DeltaCRDT, DotStore}
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._

object ORMapCRDT {
  type State[K, V, C] = Causal[DotMap[K, V], C]

  def apply[K, V: DotStore, C: CContext](replicaID: String): DeltaCRDT[State[K, V, C]] =
    DeltaCRDT.empty[State[K, V, C]](replicaID)

  def queryKey[K, V: DotStore, A, C: CContext](k: K, q: DeltaQuery[Causal[V, C], A]): DeltaQuery[State[K, V, C], A] = {
    case Causal(dm, cc) =>
      q(Causal(dm(k), cc))
  }

  def mutateKey[K, V: DotStore, C: CContext](k: K, m: DeltaMutator[Causal[V, C]]): DeltaMutator[State[K, V, C]] = {
    case (replicaID, Causal(dm, cc)) =>
      m(replicaID, Causal(dm(k), cc)) match {
      case Causal(stateDelta, ccDelta) =>
        Causal(
          DotMap[K, V].empty.updated(k, stateDelta),
          ccDelta
        )
    }
  }

  def remove[K, V: DotStore, C: CContext](k: K): DeltaMutator[State[K, V, C]] = {
    case (_, Causal(dm, _)) =>
      Causal(
        DotMap[K, V].empty,
        CContext[C].fromSet(DotStore[V].dots(dm(k)))
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
}

object ORMap {
  def apply[K, V: DotStore, C: CContext](replicaID: String): ORMap[K, V, C] =
    new ORMap(ORMapCRDT[K, V, C](replicaID))
}
