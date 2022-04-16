package kofre.decompose.interfaces

import kofre.causality.CausalContext
import kofre.decompose.DotStore.DotSet
import kofre.decompose.{CRDTInterface, UIJDLattice}
import kofre.dotbased.CausalStore
import kofre.syntax.{AllPermissionsCtx, ArdtOpsContains, DeltaMutator, DeltaQuery, OpsSyntaxHelper}

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

  implicit class GMapSyntax[C, K, V](container: C)(using aoc: ArdtOpsContains[C, GMap[K, V]])
      extends OpsSyntaxHelper[C, GMap[K, V]](container) {

    def contains(k: K)(using QueryP): Boolean = current.contains(k)

    def queryKey(k: K)(using QueryP, UIJDLattice[V]): V =
      current.getOrElse(k, UIJDLattice[V].bottom)

    def queryAllEntries()(using QueryP): Iterable[V] = current.values

    def mutateKey(k: K)(m: V => V)(using MutationIDP, UIJDLattice[V]): C = Map(k -> m(queryKey(k)))

    def mutateKeyCtx(k: K)(m: AllPermissionsCtx[V, V] => V => V)(using MutationIDP, UIJDLattice[V]): C = {
      Map(k -> m(AllPermissionsCtx.withID[V, V](replicaID))(queryKey(k)))
    }
  }

}
