package kofre.decompose.interfaces

import kofre.base.{Bottom, DecomposeLattice, Defs}
import kofre.causality.{CausalContext, Dot}
import kofre.decompose.*
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, WithNamedContext}
import kofre.contextual.ContextDecompose.*
import kofre.decompose.interfaces.MVRegisterInterface.MVRegister
import kofre.contextual.{AsCausalContext, ContextDecompose, ContextLattice, WithContext}

/** An ORMap (Observed-Remove Map) is a Delta CRDT that models a map from an arbitrary key type to nested causal Delta CRDTs.
  * In contrast to [[GMapInterface]], ORMap allows the removal of key/value pairs from the map.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
object ORMapInterface {
  type ORMap[K, V] = WithContext[Map[K, V]]

  trait ORMapCompanion {
    type State[K, V]    = ORMapInterface.ORMap[K, V]
    type Embedded[K, V] = Map[K, V]
  }

  private class DeltaStateFactory[K, V: ContextDecompose: AsCausalContext] {

    given ContextDecompose[Map[K, V]] = ContextDecompose.DotMap[K, V]

    val bottom: ORMap[K, V] = DecomposeLattice[ORMap[K, V]].empty

    def make(
        dm: Map[K, V] = bottom.store,
        cc: CausalContext = bottom.context
    ): ORMap[K, V] = WithContext(dm, cc)
  }

  private def deltaState[K, V: ContextDecompose: AsCausalContext]: DeltaStateFactory[K, V] = new DeltaStateFactory[K, V]

  implicit class ORMapSyntax[C, K, V](container: C)(using ArdtOpsContains[C, ORMap[K, V]])
      extends OpsSyntaxHelper[C, ORMap[K, V]](container) {

    def contains(k: K)(using QueryP): Boolean = current.store.contains(k)

    def queryKey[A](k: K)(using QueryP, ContextDecompose[V], AsCausalContext[V]): WithContext[V] = {
      WithContext(current.store.getOrElse(k, Bottom[V].empty), current.context)
    }

    def queryAllEntries(using QueryP): Iterable[WithContext[V]] =
      current.store.values.map(v => WithContext(v, current.context))

    def mutateKey(k: K, m: (Defs.Id, WithContext[V]) => WithContext[V])(using
        MutationIdP,
        ContextDecompose[V],
        AsCausalContext[V]
    ): C = {
      val v = current.store.getOrElse(k, Bottom[V].empty)

      m(replicaID, WithContext(v, current.context)) match {
        case WithContext(stateDelta, ccDelta) =>
          deltaState[K, V].make(
            dm = DotMap[K, V].empty.store.updated(k, stateDelta),
            cc = ccDelta
          ).mutator
      }
    }

    def mutateKeyNamedCtx(k: K)(m: WithNamedContext[V] => WithNamedContext[V])(using
        MutationIdP,
        AsCausalContext[V],
        ContextDecompose[V]
    ): C = {
      val v                                = current.store.getOrElse(k, Bottom[V].empty)
      val WithContext(stateDelta, ccDelta) = m(WithNamedContext(replicaID, WithContext(v, current.context))).anon
      deltaState[K, V].make(
        dm = DotMap[K, V].empty.store.updated(k, stateDelta),
        cc = ccDelta
      ).mutator
    }

    def remove(k: K)(using MutationIdP, ContextDecompose[V], AsCausalContext[V]): C = {
      val v = current.store.getOrElse(k, Bottom[V].empty)

      deltaState[K, V].make(
        cc = AsCausalContext[V].dots(v)
      ).mutator
    }

    def removeAll(keys: Iterable[K])(using MutationIdP, ContextDecompose[V], AsCausalContext[V]): C = {
      val values = keys.map(k => current.store.getOrElse(k, AsCausalContext[V].empty))
      val dots = values.foldLeft(CausalContext.empty) {
        case (set, v) => set union AsCausalContext[V].dots(v)
      }

      deltaState[K, V].make(
        cc = dots
      ).mutator
    }

    def removeByValue(cond: WithContext[V] => Boolean)(using MutationIdP, ContextDecompose[V], AsCausalContext[V]): C = {
      val toRemove = current.store.values.collect {
        case v if cond(WithContext(v, current.context)) => AsCausalContext[V].dots(v)
      }.fold(CausalContext.empty)(_ union _)

      deltaState[K, V].make(
        cc = toRemove
      ).mutator
    }

    def clear()(using MutationIdP, ContextDecompose[V], AsCausalContext[V]): C = {
      deltaState[K, V].make(
        cc = AsCausalContext[Map[K, V]].dots(current.store)
      ).mutator
    }
  }
}
