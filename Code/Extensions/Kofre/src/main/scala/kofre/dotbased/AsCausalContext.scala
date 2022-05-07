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
  def empty: A
}

object AsCausalContext {

  def apply[A](implicit dotStore: AsCausalContext[A]): dotStore.type = dotStore

  implicit def dotFunDotStore[V]: AsCausalContext[Map[Dot, V]] = new AsCausalContext[Map[Dot, V]] {
    override def empty: Map[Dot, V]                         = Map.empty
    override def dots(dotStore: Map[Dot, V]): CausalContext = CausalContext.fromSet(dotStore.keySet)
  }

  implicit val CausalContextDotStoreInstance: AsCausalContext[CausalContext] =
    new AsCausalContext[CausalContext] {
      override def dots(a: CausalContext): CausalContext = a
      override def empty: CausalContext                  = CausalContext.empty
    }

  implicit val DotSetInstance: AsCausalContext[Set[Dot]] =
    new AsCausalContext[Set[Dot]] {
      override def dots(a: Set[Dot]): CausalContext = CausalContext.fromSet(a)
      override def empty: Set[Dot]                  = Set.empty
    }

  implicit def DotMapInstance[Key, A: AsCausalContext]: AsCausalContext[Map[Key, A]] =
    new AsCausalContext[Map[Key, A]] {
      override def dots(a: Map[Key, A]): CausalContext =
        a.valuesIterator.foldLeft(CausalContext.empty)((acc, v) => acc.union(AsCausalContext[A].dots(v)))
      override def empty: Map[Key, A] = Map.empty
    }
}
