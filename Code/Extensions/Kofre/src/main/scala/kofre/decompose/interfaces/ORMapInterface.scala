package kofre.decompose.interfaces

import kofre.causality.{CausalContext, Dot}
import kofre.decompose.*
import kofre.syntax.{ArdtOpsContains, DeltaMutator, OpsSyntaxHelper}
import kofre.decompose.DotStore.*
import kofre.decompose.interfaces.MVRegisterInterface.MVRegister
import kofre.dotbased.CausalStore

/** An ORMap (Observed-Remove Map) is a Delta CRDT that models a map from an arbitrary key type to nested causal Delta CRDTs.
  * In contrast to [[GMapInterface]], ORMap allows the removal of key/value pairs from the map.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
object ORMapInterface {
  type ORMap[K, V] = CausalStore[DotMap[K, V]]
  type C           = CausalContext

  trait ORMapCompanion {
    type State[K, V]    = ORMapInterface.ORMap[K, V]
    type Embedded[K, V] = DotMap[K, V]
  }

  private class DeltaStateFactory[K, V: DotStore] {
    val bottom: ORMap[K, V] = UIJDLattice[ORMap[K, V]].bottom

    def make(
        dm: DotMap[K, V] = bottom.store,
        cc: C = bottom.context
    ): ORMap[K, V] = CausalStore(dm, cc)
  }

  private def deltaState[K, V: DotStore]: DeltaStateFactory[K, V] = new DeltaStateFactory[K, V]

  implicit class ORMapSyntax[C, K, V](container: C)(using ArdtOpsContains[C, ORMap[K, V]])
      extends OpsSyntaxHelper[C, ORMap[K, V]](container) {

    def contains(k: K)(using QueryP): Boolean = current.store.contains(k)

    def queryKey[A](k: K)(using QueryP, DotStore[V]): CausalStore[V] = {
      CausalStore(current.store.getOrElse(k, DotStore[V].empty), current.context)
    }

    def queryAllEntries(using QueryP): Iterable[CausalStore[V]] =
      current.store.values.map(v => CausalStore(v, current.context))

    def mutateKey(k: K, m: DeltaMutator[CausalStore[V]])(using MutationIDP,  DotStore[V]): C = {
        val v = current.store.getOrElse(k, DotStore[V].empty)

        m(replicaID, CausalStore(v, current.context)) match {
          case CausalStore(stateDelta, ccDelta) =>
            deltaState[K, V].make(
              dm = DotMap[K, V].empty.updated(k, stateDelta),
              cc = ccDelta
            )
        }
    }

    def remove(k: K)(using MutationIDP,  DotStore[V]): C = {
      val v = current.store.getOrElse(k, DotStore[V].empty)

      deltaState[K, V].make(
        cc = CausalContext.fromSet(DotStore[V].dots(v))
      )
    }

    def removeAll(keys: Iterable[K])(using MutationIDP,  DotStore[V]): C = {
      val values = keys.map(k => current.store.getOrElse(k, DotStore[V].empty))
      val dots = values.foldLeft(Set.empty[Dot]) {
        case (set, v) => set union DotStore[V].dots(v)
      }

      deltaState[K, V].make(
        cc = CausalContext.fromSet(dots)
      )
    }

    def removeByValue(cond: CausalStore[V] => Boolean)(using MutationIDP,  DotStore[V]): C = {
      val toRemove = current.store.values.collect {
        case v if cond(CausalStore(v, current.context)) => DotStore[V].dots(v)
      }.fold(Set())(_ union _)

      deltaState[K, V].make(
        cc = CausalContext.fromSet(toRemove)
      )
    }

    def clear()(using MutationIDP,  DotStore[V]): C = {
      deltaState[K, V].make(
        cc = CausalContext.fromSet(DotMap[K, V].dots(current.store))
      )
    }
  }
}
