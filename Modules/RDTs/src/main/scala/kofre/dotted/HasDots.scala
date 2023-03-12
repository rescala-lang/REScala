package kofre.dotted

import kofre.base.{Bottom, Uid, Lattice}
import kofre.time.{Dot, Dots}

import scala.compiletime.summonAll
import scala.deriving.Mirror

/** HasDots implies that the container stores values that are somehow associated to individual [[Dot]]s.
  * This is different from a dot context, which could also contain dots for deleted values or other metadata.
  * HasDots is explicitly for dots that are present/existing in some datastructure right now.
  *
  * See also: Dot stores in delta state replicated data types
  */
trait HasDots[-A] {
  def getDots(a: A): Dots

  def map[B](f: B => A): HasDots[B] = (b: B) => getDots(f(b))

  extension (a: A) def dots: Dots = getDots(a)

}

object HasDots {

  def apply[A](using dotStore: HasDots[A]): dotStore.type = dotStore

  given option[A: HasDots]: HasDots[Option[A]] =
    case None => Dots.empty
    case Some(v) => v.dots

  inline given tuple[T <: Tuple: Mirror.ProductOf]: HasDots[T] = derived

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
