package kofre.decompose.interfaces

import kofre.base.{Bottom, DecomposeLattice, Defs}
import kofre.time.{Dots, Dot}
import kofre.decompose.*
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, DottedName}
import kofre.contextual.ContextDecompose.*
import kofre.decompose.interfaces.MVRegisterInterface.MVRegister
import kofre.contextual.{AsCausalContext, ContextDecompose, ContextLattice, Dotted}
import kofre.dotted.DotMap

/** An ORMap (Observed-Remove Map) is a Delta CRDT that models a map from an arbitrary key type to nested causal Delta CRDTs.
  * In contrast to [[GrowMap]], ORMap allows the removal of key/value pairs from the map.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
object ORMapInterface {
  type ORMap[K, V] = DotMap[K, V]

  def empty[K, V]: ORMap[K, V] = DotMap.empty

  given contextDecompose[K, V: ContextDecompose: AsCausalContext]: ContextDecompose[ORMap[K, V]] = DotMap.contextDecompose


  def make[K, V](
      dm: DotMap[K, V] = DotMap.empty[K, V],
      cc: Dots = Dots.empty
  ): Dotted[ORMap[K, V]] = Dotted(dm, cc)

  implicit class ORMapSyntax[C, K, V](container: C)(using ArdtOpsContains[C, ORMap[K, V]])
      extends OpsSyntaxHelper[C, ORMap[K, V]](container) {

    def contains(k: K)(using QueryP): Boolean = current.contains(k)

    def queryKey[A](k: K)(using QueryP, CausalP, ContextDecompose[V], AsCausalContext[V]): Dotted[V] = {
      Dotted(current.getOrElse(k, Bottom[V].empty), context)
    }

    def queryAllEntries(using QueryP): Iterable[V] = current.values
    def mutateKey(k: K, m: (Defs.Id, Dotted[V]) => Dotted[V])(using
                                                              CausalMutationP,
                                                              IdentifierP,
                                                              ContextDecompose[V],
                                                              AsCausalContext[V]
    ): C = {
      val v = current.getOrElse(k, Bottom[V].empty)

      m(replicaID, context.wrap(v)) match {
        case Dotted(stateDelta, ccDelta) =>
          make[K, V](
            dm = DotMap(Map(k -> stateDelta)),
            cc = ccDelta
          ).mutator
      }
    }

    def mutateKeyNamedCtx(k: K)(m: DottedName[V] => DottedName[V])(using
                                                                   CausalMutationP,
                                                                   IdentifierP,
                                                                   AsCausalContext[V],
                                                                   ContextDecompose[V]
    ): C = {
      val v                           = current.getOrElse(k, Bottom[V].empty)
      val Dotted(stateDelta, ccDelta) = m(DottedName(replicaID, Dotted(v, context))).anon
      make[K, V](
        dm = DotMap(Map(k -> stateDelta)),
        cc = ccDelta
      ).mutator
    }

    def remove(k: K)(using CausalMutationP, ContextDecompose[V], AsCausalContext[V]): C = {
      val v = current.getOrElse(k, Bottom[V].empty)

      make[K, V](
        cc = AsCausalContext[V].dots(v)
      ).mutator
    }

    def removeAll(keys: Iterable[K])(using CausalMutationP, ContextDecompose[V], AsCausalContext[V]): C = {
      val values = keys.map(k => current.getOrElse(k, Bottom[V].empty))
      val dots = values.foldLeft(Dots.empty) {
        case (set, v) => set union AsCausalContext[V].dots(v)
      }

      make(
        cc = dots
      ).mutator
    }

    def removeByValue(cond: Dotted[V] => Boolean)(using CausalMutationP, ContextDecompose[V], AsCausalContext[V]): C = {
      val toRemove = current.values.collect {
        case v if cond(Dotted(v, context)) => AsCausalContext[V].dots(v)
      }.fold(Dots.empty)(_ union _)

      make(
        cc = toRemove
      ).mutator
    }

    def clear()(using CausalMutationP, ContextDecompose[V], AsCausalContext[V]): C = {
      make(
        cc = current.dots
      ).mutator
    }
  }
}
