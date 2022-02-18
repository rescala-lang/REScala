package kofre.decompose.interfaces

import kofre.causality.{CausalStore, CausalContext, Dot}
import kofre.decompose.*
import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose.DotStore.*

object ORMapInterface {
  type State[K, V] = CausalStore[DotMap[K, V]]
  type C = CausalContext

  trait ORMapCompanion {
    type State[K, V] = ORMapInterface.State[K, V]
    type Embedded[K, V] = DotMap[K, V]
  }

  private class DeltaStateFactory[K, V: DotStore] {
    val bottom: State[K, V] = UIJDLattice[State[K, V]].bottom

    def make(
              dm: DotMap[K, V] = bottom.store,
              cc: C = bottom.context
    ): State[K, V] = CausalStore(dm, cc)
  }

  private def deltaState[K, V: DotStore]: DeltaStateFactory[K, V] = new DeltaStateFactory[K, V]

  def contains[K, V: DotStore](k: K): DeltaQuery[State[K, V], Boolean] = {
    case CausalStore(dm, _) => dm.contains(k)
  }

  def queryKey[K, V: DotStore, A](k: K, q: DeltaQuery[CausalStore[V], A]): DeltaQuery[State[K, V], A] = {
    case CausalStore(dm, cc) =>
      val v = dm.getOrElse(k, DotStore[V].empty)
      q(CausalStore(v, cc))
  }

  def queryAllEntries[K, V: DotStore, A](q: DeltaQuery[CausalStore[V], A])
      : DeltaQuery[State[K, V], Iterable[A]] = {
    case CausalStore(dm, cc) =>
      dm.values.map(v => q(CausalStore(v, cc)))
  }

  def mutateKey[K, V: DotStore](k: K, m: DeltaMutator[CausalStore[V]]): DeltaMutator[State[K, V]] = {
    case (replicaID, CausalStore(dm, cc)) =>
      val v = dm.getOrElse(k, DotStore[V].empty)

      m(replicaID, CausalStore(v, cc)) match {
        case CausalStore(stateDelta, ccDelta) =>
          deltaState[K, V].make(
            dm = DotMap[K, V].empty.updated(k, stateDelta),
            cc = ccDelta
          )
      }
  }

  def remove[K, V: DotStore](k: K): DeltaMutator[State[K, V]] = {
    case (_, CausalStore(dm, _)) =>
      val v = dm.getOrElse(k, DotStore[V].empty)

      deltaState[K, V].make(
        cc = CausalContext.fromSet(DotStore[V].dots(v))
      )
  }

  def removeAll[K, V: DotStore](keys: Iterable[K]): DeltaMutator[State[K, V]] = {
    case (_, CausalStore(dm, _)) =>
      val values = keys.map(k => dm.getOrElse(k, DotStore[V].empty))
      val dots = values.foldLeft(Set.empty[Dot]) {
        case (set, v) => set union DotStore[V].dots(v)
      }

      deltaState[K, V].make(
        cc = CausalContext.fromSet(dots)
      )
  }

  def removeByValue[K, V: DotStore](cond: CausalStore[V] => Boolean): DeltaMutator[State[K, V]] = {
    case (_, CausalStore(dm, cc)) =>
      val toRemove = dm.values.collect {
        case v if cond(CausalStore(v, cc)) => DotStore[V].dots(v)
      }.fold(Set())(_ union _)

      deltaState[K, V].make(
        cc = CausalContext.fromSet(toRemove)
      )
  }

  def clear[K, V: DotStore]: DeltaMutator[State[K, V]] = {
    case (_, CausalStore(dm, _)) =>
      deltaState[K, V].make(
        cc = CausalContext.fromSet(DotMap[K, V].dots(dm))
      )
  }
}

/** An ORMap (Observed-Remove Map) is a Delta CRDT that models a map from an arbitrary key type to nested causal Delta CRDTs.
  * In contrast to [[GMapInterface]], ORMap allows the removal of key/value pairs from the map.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
abstract class ORMapInterface[K, V: DotStore,  Wrapper]
    extends CRDTInterface[ORMapInterface.State[K, V], Wrapper] {

  def queryKey[A](k: K, q: DeltaQuery[CausalStore[V], A]): A = query(ORMapInterface.queryKey(k, q))

  def contains(k: K): Boolean = query(ORMapInterface.contains(k))

  def queryAllEntries[A](q: DeltaQuery[CausalStore[V], A]): Iterable[A] = query(ORMapInterface.queryAllEntries(q))

  def mutateKey(k: K, m: DeltaMutator[CausalStore[V]]): Wrapper =
    mutate(ORMapInterface.mutateKey(k, m))

  def remove(k: K): Wrapper = mutate(ORMapInterface.remove(k))

  def removeAll(keys: Iterable[K]): Wrapper = mutate(ORMapInterface.removeAll(keys))

  def removeByValue(cond: CausalStore[V] => Boolean): Wrapper =
    mutate(ORMapInterface.removeByValue(cond))

  def clear(): Wrapper = mutate(ORMapInterface.clear)
}
