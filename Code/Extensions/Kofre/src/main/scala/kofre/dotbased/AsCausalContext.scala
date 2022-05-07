package kofre.dotbased

import kofre.Defs.Id
import kofre.Lattice
import kofre.causality.{CausalContext, Dot}

/** See: Dot stores in delta state replicated data types
  *
  * But here, a dot store is something that can be seen as a CausalContext
  */
trait AsCausalContext[A] {
  def dots(a: A): CausalContext
  extension (a: A) def asContext: CausalContext = dots(a)
  def empty: A
}

object AsCausalContext {

  def apply[A](implicit dotStore: AsCausalContext[A]): dotStore.type = dotStore

  given dotFunDotStore[V]: AsCausalContext[Map[Dot, V]] with {
    override def empty: Map[Dot, V]                         = Map.empty
    override def dots(dotStore: Map[Dot, V]): CausalContext = CausalContext.fromSet(dotStore.keySet)
  }

  given CausalContextDotStoreInstance: AsCausalContext[CausalContext] with {
    override def dots(a: CausalContext): CausalContext = a
    override def empty: CausalContext                  = CausalContext.empty
  }

  given DotSetInstance: AsCausalContext[Set[Dot]] with {
    override def dots(a: Set[Dot]): CausalContext = CausalContext.fromSet(a)
    override def empty: Set[Dot]                  = Set.empty
  }

  given DotMapInstance[Key, A: AsCausalContext]: AsCausalContext[Map[Key, A]] with {
    override def dots(a: Map[Key, A]): CausalContext =
      a.valuesIterator.foldLeft(CausalContext.empty)((acc, v) => acc.union(AsCausalContext[A].dots(v)))
    override def empty: Map[Key, A] = Map.empty
  }
}
