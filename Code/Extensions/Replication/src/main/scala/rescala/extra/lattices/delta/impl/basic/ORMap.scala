package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.{CContext, Causal, DeltaMutator, DeltaQuery, DotStore}
import rescala.extra.lattices.delta.DotStore.DotMap
import rescala.extra.lattices.delta.crdt.ORMapCRDT

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
    new ORMap(DeltaCRDT.empty(antiEntropy))
}
