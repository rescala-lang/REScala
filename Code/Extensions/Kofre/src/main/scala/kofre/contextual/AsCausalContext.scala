package kofre.contextual

import kofre.base.Defs.Id
import kofre.base.Lattice
import kofre.base.Bottom
import kofre.causality.{CausalContext, Dot}

import scala.compiletime.summonAll
import scala.deriving.Mirror

/** See: Dot stores in delta state replicated data types
  *
  * But here, a dot store is something that can be seen as a CausalContext
  */
trait AsCausalContext[A] extends Bottom[A] {
  def dots(a: A): CausalContext
}

object AsCausalContext {

  def apply[A](using dotStore: AsCausalContext[A]): dotStore.type = dotStore



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
      AsCausalContext[A].dots(ds1) union AsCausalContext[B].dots(ds2)
    override def empty: (A, B) = (Bottom.empty[A], Bottom.empty[B])
  }

  inline given derived[T <: Product](using pm: Mirror.ProductOf[T]): AsCausalContext[T] =
    val lattices =
      summonAll[Tuple.Map[pm.MirroredElemTypes, AsCausalContext]].toIArray.map(_.asInstanceOf[AsCausalContext[Any]])
    ProductAsCausalContext(pm, lattices)

  class ProductAsCausalContext[T <: Product](pm: Mirror.ProductOf[T], children: IArray[AsCausalContext[Any]])
      extends AsCausalContext[T] {
    override def dots(a: T): CausalContext = Range(0, a.productArity).foldLeft(CausalContext.empty) { (c, i) =>
      c.union(children(i).dots(a.productElement(i)))
    }

    override def empty: T =
      pm.fromProduct(
        Tuple.fromIArray(children.map(_.empty))
      )

  }
}
