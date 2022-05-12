package kofre.decompose.interfaces

import kofre.base.DecomposeLattice
import kofre.causality.CausalContext
import kofre.contextual.ContextDecompose.DotSet
import kofre.contextual.{AsCausalContext, ContextDecompose, ContextLattice, WithContext}
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermIdMutate, WithNamedContext}

/** A GMap (Grow-only Map) is a Delta CRDT that models a map from an arbitrary key type to nested Delta CRDTs.
  * In contrast to [[ORMapInterface]], key/value pairs cannot be removed from this map. However, due to the smaller internal
  * representation, mutate operations on large maps are a lot faster than on ORMap.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
case class GMap[K, V](inner: Map[K, V])

object GMap {

  def empty[K, V]: GMap[K, V] = GMap(Map.empty)

  given contextLattice[K, V: ContextDecompose: AsCausalContext]: ContextDecompose[GMap[K, V]] = ContextDecompose.derived[GMap[K, V]]

  implicit class GMapSyntax[C, K, V](container: C)(using aoc: ArdtOpsContains[C, GMap[K, V]])
      extends OpsSyntaxHelper[C, GMap[K, V]](container) {

    def contains(k: K)(using QueryP): Boolean = current.inner.contains(k)

    def queryKey(k: K)(using QueryP): Option[V] = current.inner.get(k)

    def queryAllEntries()(using QueryP): Iterable[V] = current.inner.values

    def mutateKey(k: K)(m: Option[V] => V)(using MutationIdP): C = GMap(Map(k -> m(queryKey(k)))).mutator

    def mutateKeyCtx(k: K)(m: PermIdMutate[V, V] => Option[V] => V)(using MutationIdP): C = {
      GMap(Map(k -> m(PermIdMutate.withID[V, V](replicaID))(queryKey(k)))).mutator
    }

    def mutateKeyNamedCtx(k: K, default: => V)(m: WithNamedContext[V] => WithNamedContext[V])(using CausalMutationP, IdentifierP): C = {
      m(WithContext(queryKey(k).getOrElse(default), context).named(replicaID)).anon.map(v => GMap(Map(k -> v))).mutator
    }
  }

}
