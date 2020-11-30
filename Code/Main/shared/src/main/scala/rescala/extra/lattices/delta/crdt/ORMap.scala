package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.{CContext, DeltaCRDT, DotStore, SetDelta}
import rescala.extra.lattices.delta.DotStore._

object ORMap {
  def apply[K, V: DotStore, C: CContext](replicaID: String): DeltaCRDT[DotMap[K, V], C] =
    DeltaCRDT(replicaID, DotMap[K, V].bottom, CContext[C].empty, List())

  def mutateKey[K, V: DotStore](k: K, deltaMutator: DeltaDotMutator[V]): DeltaDotMutator[DotMap[K, V]] = (dm, nextDot) =>
    deltaMutator(dm.getOrElse(k, DotStore[V].bottom), nextDot) match {
      case SetDelta(state, dots) =>
        SetDelta(Map(k -> state), dots)
    }

  def mutateKey[K, V: DotStore](k: K, deltaMutator: DeltaMutator[V]): DeltaMutator[DotMap[K, V]] = dm =>
    deltaMutator(dm.getOrElse(k, DotStore[V].bottom)) match {
      case SetDelta(state, dots) =>
        SetDelta(Map(k -> state), dots)
    }

  def queryKey[K, V: DotStore, A](k: K, q: DeltaQuery[V, A]): DeltaQuery[DotMap[K, V], A] = dm =>
    q(dm.getOrElse(k, DotStore[V].bottom))

  def remove[K, V: DotStore](k: K): DeltaMutator[DotMap[K, V]] = dm =>
    SetDelta(DotMap[K, V].bottom, DotStore[V].dots(dm.getOrElse(k, DotStore[V].bottom)))

  def clear[K, V: DotStore]: DeltaMutator[DotMap[K, V]] = dm =>
    SetDelta(DotMap[K, V].bottom, DotMap[K, V].dots(dm))
}
