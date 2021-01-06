package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.{CContext, Causal, DeltaCRDT, DotStore}
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._

object ORMap {
  type State[K, V, C] = Causal[DotMap[K, V], C]

  def apply[K, V: DotStore, C: CContext](replicaID: String): DeltaCRDT[State[K, V, C]] =
    DeltaCRDT.empty[State[K, V, C]](replicaID)

  def mutateKey[K, V: DotStore, C: CContext](k: K, deltaMutator: DeltaMutator[Causal[V, C]]): DeltaMutator[State[K, V, C]] = {
    case (replicaID, Causal(dm, cc)) =>
      deltaMutator(replicaID, Causal(dm(k), cc)) match {
      case Causal(stateDelta, ccDelta) =>
        Causal(
          DotMap[K, V].empty.updated(k, stateDelta),
          ccDelta
        )
    }
  }

  def queryKey[K, V: DotStore, A, C: CContext](k: K, q: DeltaQuery[Causal[V, C], A]): DeltaQuery[State[K, V, C], A] = {
    case Causal(dm, cc) =>
      q(Causal(dm(k), cc))
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
