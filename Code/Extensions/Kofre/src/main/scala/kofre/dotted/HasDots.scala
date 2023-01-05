package kofre.dotted

import kofre.base.Id
import kofre.base.{Bottom, Lattice}
import kofre.time.{Dot, Dots}

import scala.compiletime.summonAll
import scala.deriving.Mirror

/** See: Dot stores in delta state replicated data types
  *
  * But here, a dot store is something that can be seen as a Dots
  */
trait HasDots[A] {
  def dots(a: A): Dots
}

object HasDots {

  def apply[A](using dotStore: HasDots[A]): dotStore.type = dotStore

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): HasDots[T] =
    val lattices =
      summonAll[Tuple.Map[pm.MirroredElemTypes, HasDots]].toIArray.map(_.asInstanceOf[HasDots[Any]])
    new ProductHasDots(pm, lattices)

  class ProductHasDots[T <: Product](pm: Mirror.ProductOf[T], children: IArray[HasDots[Any]])
      extends HasDots[T] {
    override def dots(a: T): Dots = Range(0, a.productArity).foldLeft(Dots.empty) { (c, i) =>
      c.union(children(i).dots(a.productElement(i)))
    }

  }
}
