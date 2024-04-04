package rdts.datatypes

import rdts.base.{Bottom, Lattice}
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.datatypes.contextual.ObserveRemoveMap.make
import rdts.dotted.Dotted
import rdts.syntax.OpsSyntaxHelper
import rdts.syntax.LocalReplicaId
import rdts.time.Dots

/** A GMap (Grow-only Map) is a Delta CRDT that models a map from an arbitrary key type to nested Delta CRDTs.
  * In contrast to [[rdts.datatypes.contextual.ObserveRemoveMap]], key/value pairs cannot be removed from this map. However, due to the smaller internal
  * representation, mutate operations on large maps are a lot faster than on ObserveRemoveMap.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
type GrowOnlyMap[K, V] = Map[K, V]

object GrowOnlyMap {

  def empty[K, V]: GrowOnlyMap[K, V] = Map.empty

  given bottom[K, V]: Bottom[GrowOnlyMap[K, V]] = Bottom.mapBottom

  given lattice[K, V: Lattice]: Lattice[GrowOnlyMap[K, V]] = Lattice.mapLattice

  extension [C, K, V](container: C)
    def growOnlyMap: syntax[C, K, V] = syntax(container)

  implicit class syntax[C, K, V](container: C)
      extends OpsSyntaxHelper[C, GrowOnlyMap[K, V]](container) {

    def contains(using IsQuery)(k: K): Boolean = current.contains(k)

    def queryKey(using IsQuery)(k: K): Option[V] = current.get(k)

    def queryAllEntries(using IsQuery)(): Iterable[V] = current.values

    def mutateKeyNamedCtx(k: K, default: => V)(m: Dotted[V] => Dotted[V])(using
        IsCausalMutator,
    ): C = {
      m(
        container.queryKey(k).getOrElse(default).inheritContext
      ).map(v => Map(k -> v)).mutator
    }
    def +(element: (K, V))(using IsCausalMutator, Bottom[V]): C =
      // mutateKeyNamedCtx(element._1, Bottom.empty[V])(_ => Dotted(element._2, Dots.single(context.nextDot(replicaId))))
      mutateKeyNamedCtx(element._1, Bottom.empty[V])(_ => element._2.inheritContext)
  }

}
