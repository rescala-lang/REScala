package kofre.dotted

import kofre.time.Dots

import scala.compiletime.summonAll
import scala.deriving.Mirror
import scala.util.control.ControlThrowable

/** HasDots implies that the container stores values that are somehow associated to individual [[kofre.time.Dot]]s.
  * This is different from a dot context, which could also contain dots for deleted values or other metadata.
  * HasDots is explicitly for dots that are present/existing in some datastructures right now.
  *
  * See also: Dot stores in delta state replicated data types
  */
trait HasDots[A] {
  extension (dotted: A)
    def dots: Dots

    /** Removes dots and anything associated to them from the value.
      * In case the value becomes fully “empty” returns None
      */
    def removeDots(dots: Dots): Option[A]
}

object HasDots {

  def apply[A](using dotStore: HasDots[A]): dotStore.type = dotStore

  def noDots[A]: HasDots[A] = new {
    extension (dotted: A)
      def dots: Dots                        = Dots.empty
      def removeDots(dots: Dots): Option[A] = Some(dotted)
  }

  inline given setInstance[A]: HasDots[Set[A]] =
    val kdots = scala.compiletime.summonFrom:
      case hd: HasDots[A] => Some(hd)
      case _              => None
    new {
      extension (dotted: Set[A])
        def dots: Dots                        = kdots.iterator.flatMap(kd => dotted.map(kd.dots)).foldLeft(Dots.empty)(_ union _)
        def removeDots(dots: Dots): Option[Set[A]] =
          val res = dotted.iterator.flatMap: elem =>
            kdots.map(kd => kd.removeDots(elem)(dots)).getOrElse(Some(elem))
          .toSet
          if res.isEmpty then None else Some(res)
    }

  inline given mapInstance[K, V]: HasDots[Map[K, V]] =
    val kdots = scala.compiletime.summonFrom:
      case hd: HasDots[K] => Some(hd)
      case _              => None
    val vdots = scala.compiletime.summonFrom:
      case hd: HasDots[V] => Some(hd)
      case _              => None
    new HasDots[Map[K, V]] {
      extension (dotted: Map[K, V])
        def dots: Dots =
          val all = kdots.iterator.flatMap(d => dotted.keysIterator.map(d.dots))
            ++ vdots.iterator.flatMap(d => dotted.valuesIterator.map(d.dots))
          all.foldLeft(Dots.empty)(_ union _)

        def removeDots(dots: Dots): Option[Map[K, V]] =
          val res = dotted.iterator.flatMap: (k, v) =>
            val nk = kdots.map(d => d.removeDots(k)(dots)).getOrElse(Some(k))
            val nv = vdots.map(d => d.removeDots(v)(dots)).getOrElse(Some(v))
            nk.zip(nv)
          .toMap
          if res.isEmpty then None else Some(res)
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
