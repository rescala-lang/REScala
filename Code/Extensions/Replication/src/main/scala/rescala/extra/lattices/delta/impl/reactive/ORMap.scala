package rescala.extra.lattices.delta.impl.reactive

import rescala.extra.lattices.delta.{CContext, Causal, Delta, DeltaMutator, DeltaQuery, DotStore}
import rescala.extra.lattices.delta.DotStore.DotMap
import rescala.extra.lattices.delta.crdt.ORMapCRDT
import rescala.extra.lattices.delta.crdt.ORMapCRDT.State

class ORMap[K, V: DotStore, C: CContext](val crdt: DeltaCRDT[ORMapCRDT.State[K, V, C]])
    extends CRDTInterface[ORMapCRDT.State[K, V, C]] {
  def queryKey[A](k: K, q: DeltaQuery[Causal[V, C], A]): A = crdt.query(ORMapCRDT.queryKey(k, q))

  def contains(k: K): Boolean = crdt.query(ORMapCRDT.contains(k))

  def queryAllEntries[A](q: DeltaQuery[Causal[V, C], A]): Iterable[A] = crdt.query(ORMapCRDT.queryAllEntries(q))

  def mutateKey(k: K, m: DeltaMutator[Causal[V, C]]): ORMap[K, V, C] =
    new ORMap(crdt.mutate(ORMapCRDT.mutateKey(k, m)))

  def remove(k: K): ORMap[K, V, C] = new ORMap(crdt.mutate(ORMapCRDT.remove(k)))

  def removeAll(keys: Iterable[K]): ORMap[K, V, C] = new ORMap(crdt.mutate(ORMapCRDT.removeAll(keys)))

  def removeByValue(cond: Causal[V, C] => Boolean): ORMap[K, V, C] =
    new ORMap(crdt.mutate(ORMapCRDT.removeByValue(cond)))

  def clear(): ORMap[K, V, C] = new ORMap(crdt.mutate(ORMapCRDT.clear))

  def applyDelta(delta: Delta[State[K, V, C]]): ORMap[K, V, C] = {
    val newCRDT = crdt.applyDelta(delta)
    if (newCRDT == crdt) this else new ORMap(newCRDT)
  }
}

object ORMap {
  type State[K, V, C] = ORMapCRDT.State[K, V, C]
  type Embedded[K, V] = DotMap[K, V]

  def apply[K, V: DotStore, C: CContext](replicaID: String): ORMap[K, V, C] =
    new ORMap(DeltaCRDT.empty(replicaID))
}
