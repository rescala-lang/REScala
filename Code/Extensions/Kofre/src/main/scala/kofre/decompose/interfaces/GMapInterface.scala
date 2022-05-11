package kofre.decompose.interfaces

import kofre.base.DecomposeLattice
import kofre.causality.CausalContext
import kofre.contextual.ContextDecompose.DotSet
import kofre.contextual.{ContextDecompose, ContextLattice, WithContext}
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermIdMutate, WithNamedContext}

/** A GMap (Grow-only Map) is a Delta CRDT that models a map from an arbitrary key type to nested Delta CRDTs.
  * In contrast to [[ORMapInterface]], key/value pairs cannot be removed from this map. However, due to the smaller internal
  * representation, mutate operations on large maps are a lot faster than on ORMap.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
object GMapInterface {
  type GMap[K, V] = Map[K, V]

  trait GMapCompanion {
    type State[K, V] = GMapInterface.GMap[K, V]
  }


  def empty[K, V]: GMap[K, V] = Map.empty

  implicit class GMapSyntax[C, K, V](container: C)(using aoc: ArdtOpsContains[C, GMap[K, V]])
      extends OpsSyntaxHelper[C, GMap[K, V]](container) {

    def contains(k: K)(using QueryP): Boolean = current.contains(k)

    def queryKey(k: K)(using QueryP, ContextDecompose[V]): V =
      current.getOrElse(k, ContextDecompose[V].empty.store)

    def queryAllEntries()(using QueryP): Iterable[V] = current.values

    def mutateKey(k: K)(m: V => V)(using MutationIdP, ContextDecompose[V]): C = Map(k -> m(queryKey(k))).mutator

    def mutateKeyCtx(k: K)(m: PermIdMutate[V, V] => V => V)(using MutationIdP, ContextDecompose[V]): C = {
      Map(k -> m(PermIdMutate.withID[V, V](replicaID))(queryKey(k))).mutator
    }

    def mutateKeyNamedCtx(k: K)(m: WithNamedContext[V] => WithNamedContext[V])(using CausalMutationP, IdentifierP, ContextDecompose[V]): C = {
      m(WithNamedContext(replicaID, WithContext(queryKey(k), context))).anon.map(v => Map(k -> v)).mutator
    }
  }

}
