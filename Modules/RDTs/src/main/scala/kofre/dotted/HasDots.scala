package kofre.dotted

import kofre.base.{Bottom, Uid, Lattice}
import kofre.time.{Dot, Dots}

import scala.compiletime.summonAll
import scala.deriving.Mirror

/** See: Dot stores in delta state replicated data types
  *
  * But here, a dot store is something that can be seen as a Dots
  */
trait HasDots[-A] {
  def getDots(a: A): Dots

  def map[B](f: B => A): HasDots[B] = (b: B) => getDots(f(b))

  extension [A: HasDots](a: A) def dots: Dots = summon.getDots(a)

}

object HasDots {

  def apply[A](using dotStore: HasDots[A]): dotStore.type = dotStore

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): HasDots[T] =
    val lattices =
      summonAll[Tuple.Map[pm.MirroredElemTypes, HasDots]].toIArray.map(_.asInstanceOf[HasDots[Any]])
    new ProductHasDots(pm, lattices)

  class ProductHasDots[T <: Product](pm: Mirror.ProductOf[T], children: IArray[HasDots[Any]])
      extends HasDots[T] {
    override def getDots(a: T): Dots = Range(0, a.productArity).foldLeft(Dots.empty) { (c, i) =>
      c.union(children(i).getDots(a.productElement(i)))
    }

  }
}
