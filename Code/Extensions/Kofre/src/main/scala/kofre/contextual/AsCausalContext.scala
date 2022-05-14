package kofre.contextual

import kofre.base.Defs.Id
import kofre.base.Lattice
import kofre.base.Bottom
import kofre.time.{Dots, Dot}

import scala.compiletime.summonAll
import scala.deriving.Mirror

/** See: Dot stores in delta state replicated data types
  *
  * But here, a dot store is something that can be seen as a Dots
  */
trait AsCausalContext[A] {
  def dots(a: A): Dots
}

object AsCausalContext {

  def apply[A](using dotStore: AsCausalContext[A]): dotStore.type = dotStore

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): AsCausalContext[T] =
    val lattices =
      summonAll[Tuple.Map[pm.MirroredElemTypes, AsCausalContext]].toIArray.map(_.asInstanceOf[AsCausalContext[Any]])
    ProductAsCausalContext(pm, lattices)

  class ProductAsCausalContext[T <: Product](pm: Mirror.ProductOf[T], children: IArray[AsCausalContext[Any]])
      extends AsCausalContext[T] {
    override def dots(a: T): Dots = Range(0, a.productArity).foldLeft(Dots.empty) { (c, i) =>
      c.union(children(i).dots(a.productElement(i)))
    }

  }
}
