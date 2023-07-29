package kofre.dotted


import kofre.time.Dots

import scala.compiletime.summonAll
import scala.deriving.Mirror
import scala.util.control.ControlThrowable

/** HasDots implies that the container stores values that are somehow associated to individual [[Dot]]s.
  * This is different from a dot context, which could also contain dots for deleted values or other metadata.
  * HasDots is explicitly for dots that are present/existing in some datastructures right now.
  *
  * See also: Dot stores in delta state replicated data types
  */
trait HasDots[A] {
  extension (dotted: A)
    def dots: Dots
    def removeDots(dots: Dots): Option[A]
}

object HasDots {

  def apply[A](using dotStore: HasDots[A]): dotStore.type = dotStore

  def noDots[A]: HasDots[A] = new {
    extension (dotted: A)
      def dots: Dots = Dots.empty
      def removeDots(dots: Dots): Option[A] = Some(dotted)
  }

  given option[A: HasDots]: HasDots[Option[A]] with
    extension (dotted: Option[A])
      def dots: Dots = dotted match
        case None    => Dots.empty
        case Some(v) => HasDots[A].dots(v)
      def removeDots(dots: Dots): Option[Option[A]] = dotted match
        case None    => None
        case Some(v) => v.removeDots(dots).map(Some.apply)

  inline given tuple[T <: Tuple: Mirror.ProductOf]: HasDots[T] = derived

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): HasDots[T] =
    val lattices =
      summonAll[Tuple.Map[pm.MirroredElemTypes, HasDots]].toIArray.map(_.asInstanceOf[HasDots[Any]])
    new ProductHasDots(pm, lattices)

  class ProductHasDots[T <: Product](pm: Mirror.ProductOf[T], children: IArray[HasDots[Any]]) extends HasDots[T] {
    extension (a: T)
      def dots: Dots = Range(0, a.productArity).foldLeft(Dots.empty) { (c, i) =>
        c.union(children(i).dots(a.productElement(i)))
      }
      def removeDots(dots: Dots): Option[T] =
        object FilterControl extends ControlThrowable
        try
          Some(pm.fromProduct(new Product {
            def canEqual(that: Any): Boolean = false
            def productArity: Int            = children.size
            def productElement(i: Int): Any =
              children(i).removeDots(a.productElement(i))(dots).getOrElse {
                throw FilterControl
              }
          }))
        catch case FilterControl => None

  }
}
