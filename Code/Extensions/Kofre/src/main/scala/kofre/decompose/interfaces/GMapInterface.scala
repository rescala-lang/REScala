package kofre.decompose.interfaces

import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose.{CRDTInterface, UIJDLattice}

object GMapInterface {
  type State[K, V] = Map[K, V]

  trait GMapCompanion {
    type State[K, V] = GMapInterface.State[K, V]
  }

  def contains[K, V: UIJDLattice](k: K): DeltaQuery[State[K, V], Boolean] = state => state.contains(k)

  def queryKey[K, V: UIJDLattice, A](k: K, q: DeltaQuery[V, A]): DeltaQuery[State[K, V], A] = state =>
    q(state.getOrElse(k, UIJDLattice[V].bottom))

  def queryAllEntries[K, V: UIJDLattice, A](q: DeltaQuery[V, A]): DeltaQuery[State[K, V], Iterable[A]] = state =>
    state.values.map(q)

  def mutateKey[K, V: UIJDLattice](k: K, m: DeltaMutator[V]): DeltaMutator[State[K, V]] = (replicaID, state) => {
    val v = state.getOrElse(k, UIJDLattice[V].bottom)

    val vDelta = m(replicaID, v)

    Map(k -> vDelta)
  }
}

/** A GMap (Grow-only Map) is a Delta CRDT that models a map from an arbitrary key type to nested Delta CRDTs.
  * In contrast to [[ORMapInterface]], key/value pairs cannot be removed from this map. However, due to the smaller internal
  * representation, mutate operations on large maps are a lot faster than on ORMap.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
abstract class GMapInterface[K, V: UIJDLattice, Wrapper] extends CRDTInterface[GMapInterface.State[K, V], Wrapper] {

  def contains(k: K): Boolean = query(GMapInterface.contains(k))

  def queryKey[A](k: K, q: DeltaQuery[V, A]): A = query(GMapInterface.queryKey(k, q))

  def queryAllEntries[A](q: DeltaQuery[V, A]): Iterable[A] = query(GMapInterface.queryAllEntries(q))

  def mutateKey(k: K, m: DeltaMutator[V]): Wrapper = mutate(GMapInterface.mutateKey(k, m))
}
