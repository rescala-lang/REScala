package kofre.datatypes

import kofre.base.{Bottom, DecomposeLattice}
import kofre.dotted.{DotMap, Dotted, DottedDecompose, DottedLattice, HasDots}
import kofre.syntax.{ArdtOpsContains, DottedName, OpsSyntaxHelper, PermIdMutate}
import kofre.time.Dots

/** A GMap (Grow-only Map) is a Delta CRDT that models a map from an arbitrary key type to nested Delta CRDTs.
  * In contrast to [[ObserveRemoveMap]], key/value pairs cannot be removed from this map. However, due to the smaller internal
  * representation, mutate operations on large maps are a lot faster than on ObserveRemoveMap.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
type GrowOnlyMap[K, V] = Map[K, V]

object GrowOnlyMap {

  def empty[K, V]: GrowOnlyMap[K, V] = Map.empty

  given [K, V]: Bottom[GrowOnlyMap[K, V]] = Bottom.mapBottom

  given decomposeLattice[K, V: DecomposeLattice]: DecomposeLattice[GrowOnlyMap[K, V]] = DecomposeLattice.mapLattice
  given contextLattice[K, V: DottedDecompose: HasDots: Bottom]: DottedDecompose[GrowOnlyMap[K, V]] =
    DotMap.contextDecompose[K, V].contextbimap[Map[K, V]](_.map(_.repr), _.map(DotMap.apply))

  implicit class GMapSyntax[C, K, V](container: C)(using aoc: ArdtOpsContains[C, GrowOnlyMap[K, V]])
      extends OpsSyntaxHelper[C, GrowOnlyMap[K, V]](container) {

    def contains(k: K)(using QueryP): Boolean = current.contains(k)

    def queryKey(k: K)(using QueryP): Option[V] = current.get(k)

    def queryAllEntries()(using QueryP): Iterable[V] = current.values

    def mutateKeyNamedCtx(k: K, default: => V)(m: DottedName[V] => DottedName[V])(using
        CausalMutationP,
        IdentifierP
    ): C = {
      m(
        queryKey(k).getOrElse(default).inheritContext
      ).anon.map(v => Map(k -> v)).mutator
    }
  }

}
