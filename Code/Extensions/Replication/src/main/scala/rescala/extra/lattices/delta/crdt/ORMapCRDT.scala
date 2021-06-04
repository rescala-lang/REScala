package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta._

object ORMapCRDT {
  type State[K, V, C] = Causal[DotMap[K, V], C]

  private def deltaState[K, V: DotStore, C: CContext](
      dm: Option[DotMap[K, V]] = None,
      cc: C
  ): State[K, V, C] = {
    val bottom = UIJDLattice[State[K, V, C]].bottom

    Causal(
      dm.getOrElse(bottom.dotStore),
      cc
    )
  }

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
          deltaState(
            dm = Some(DotMap[K, V].empty.updated(k, stateDelta)),
            cc = ccDelta
          )
      }
  }

  def remove[K, V: DotStore, C: CContext](k: K): DeltaMutator[State[K, V, C]] = {
    case (_, Causal(dm, _)) =>
      val v = dm.getOrElse(k, DotStore[V].empty)

      deltaState(
        cc = CContext[C].fromSet(DotStore[V].dots(v))
      )
  }

  def removeAll[K, V: DotStore, C: CContext](keys: Iterable[K]): DeltaMutator[State[K, V, C]] = {
    case (_, Causal(dm, _)) =>
      val values = keys.map(k => dm.getOrElse(k, DotStore[V].empty))
      val dots = values.foldLeft(Set.empty[Dot]) {
        case (set, v) => set union DotStore[V].dots(v)
      }

      deltaState(
        cc = CContext[C].fromSet(dots)
      )
  }

  def removeByValue[K, V: DotStore, C: CContext](cond: Causal[V, C] => Boolean): DeltaMutator[State[K, V, C]] = {
    case (_, Causal(dm, cc)) =>
      val toRemove = dm.values.collect {
        case v if cond(Causal(v, cc)) => DotStore[V].dots(v)
      }.fold(Set())(_ union _)

      deltaState(
        cc = CContext[C].fromSet(toRemove)
      )
  }

  def clear[K, V: DotStore, C: CContext]: DeltaMutator[State[K, V, C]] = {
    case (_, Causal(dm, _)) =>
      deltaState(
        cc = CContext[C].fromSet(DotMap[K, V].dots(dm))
      )
  }
}
