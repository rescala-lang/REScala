package kofre.dotbased

import kofre.Defs.Id
import kofre.Lattice
import kofre.causality.{CausalContext, Dot}
import kofre.Bottom

/** See: Dot stores in delta state replicated data types
  *
  * But here, a dot store is something that can be seen as a CausalContext
  */
trait AsCausalContext[A] extends Bottom[A] {
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

  given causalContextInstance: AsCausalContext[CausalContext] with {
    override def dots(a: CausalContext): CausalContext = a
    override def empty: CausalContext                  = CausalContext.empty
  }

  given DotSetInstance: AsCausalContext[Set[Dot]] with {
    override def dots(a: Set[Dot]): CausalContext = CausalContext.fromSet(a)
    override def empty: Set[Dot]                  = Set.empty
  }

  given DotMapInstance[K, A: AsCausalContext]: AsCausalContext[Map[K, A]] with {
    override def dots(a: Map[K, A]): CausalContext =
      a.valuesIterator.foldLeft(CausalContext.empty)((acc, v) => acc.union(AsCausalContext[A].dots(v)))
    override def empty: Map[K, A] = Map.empty
  }

  given DotPairInstance[A: AsCausalContext, B: AsCausalContext]: AsCausalContext[(A, B)] with {
    override def dots(ds: (A, B)): CausalContext =
      val (ds1, ds2) = ds
      ds1.asContext union ds2.asContext
    override def empty: (A, B) = (Bottom.empty[A], Bottom.empty[B])
  }
}
