package kofre.decompose.interfaces

import kofre.decompose.*
import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose.DotStore.*
import kofre.causality.Dot

object ORMapInterface {
  type State[K, V, C] = Causal[DotMap[K, V], C]

  trait ORMapCompanion {
    type State[K, V, C] = ORMapInterface.State[K, V, C]
    type Embedded[K, V] = DotMap[K, V]
  }

  private class DeltaStateFactory[K, V: DotStore, C: CContext] {
    val bottom: State[K, V, C] = UIJDLattice[State[K, V, C]].bottom

    def make(
        dm: DotMap[K, V] = bottom.dotStore,
        cc: C = bottom.cc
    ): State[K, V, C] = Causal(dm, cc)
  }

  private def deltaState[K, V: DotStore, C: CContext]: DeltaStateFactory[K, V, C] = new DeltaStateFactory[K, V, C]

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
          deltaState[K, V, C].make(
            dm = DotMap[K, V].empty.updated(k, stateDelta),
            cc = ccDelta
          )
      }
  }

  def remove[K, V: DotStore, C: CContext](k: K): DeltaMutator[State[K, V, C]] = {
    case (_, Causal(dm, _)) =>
      val v = dm.getOrElse(k, DotStore[V].empty)

      deltaState[K, V, C].make(
        cc = CContext[C].fromSet(DotStore[V].dots(v))
      )
  }

  def removeAll[K, V: DotStore, C: CContext](keys: Iterable[K]): DeltaMutator[State[K, V, C]] = {
    case (_, Causal(dm, _)) =>
      val values = keys.map(k => dm.getOrElse(k, DotStore[V].empty))
      val dots = values.foldLeft(Set.empty[Dot]) {
        case (set, v) => set union DotStore[V].dots(v)
      }

      deltaState[K, V, C].make(
        cc = CContext[C].fromSet(dots)
      )
  }

  def removeByValue[K, V: DotStore, C: CContext](cond: Causal[V, C] => Boolean): DeltaMutator[State[K, V, C]] = {
    case (_, Causal(dm, cc)) =>
      val toRemove = dm.values.collect {
        case v if cond(Causal(v, cc)) => DotStore[V].dots(v)
      }.fold(Set())(_ union _)

      deltaState[K, V, C].make(
        cc = CContext[C].fromSet(toRemove)
      )
  }

  def clear[K, V: DotStore, C: CContext]: DeltaMutator[State[K, V, C]] = {
    case (_, Causal(dm, _)) =>
      deltaState[K, V, C].make(
        cc = CContext[C].fromSet(DotMap[K, V].dots(dm))
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
abstract class ORMapInterface[K, V: DotStore, C: CContext, Wrapper]
    extends CRDTInterface[ORMapInterface.State[K, V, C], Wrapper] {

  def queryKey[A](k: K, q: DeltaQuery[Causal[V, C], A]): A = query(ORMapInterface.queryKey(k, q))

  def contains(k: K): Boolean = query(ORMapInterface.contains(k))

  def queryAllEntries[A](q: DeltaQuery[Causal[V, C], A]): Iterable[A] = query(ORMapInterface.queryAllEntries(q))

  def mutateKey(k: K, m: DeltaMutator[Causal[V, C]]): Wrapper =
    mutate(ORMapInterface.mutateKey(k, m))

  def remove(k: K): Wrapper = mutate(ORMapInterface.remove(k))

  def removeAll(keys: Iterable[K]): Wrapper = mutate(ORMapInterface.removeAll(keys))

  def removeByValue(cond: Causal[V, C] => Boolean): Wrapper =
    mutate(ORMapInterface.removeByValue(cond))

  def clear(): Wrapper = mutate(ORMapInterface.clear)
}
