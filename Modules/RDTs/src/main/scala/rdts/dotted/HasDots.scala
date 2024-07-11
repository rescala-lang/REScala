package rdts.dotted

import rdts.base.Bottom
import rdts.time.Dots

import scala.compiletime.summonAll
import scala.deriving.Mirror

/** HasDots implies that the container stores values that are somehow associated to individual [[rdts.time.Dot]]s.
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

  class HasDotsSet[A](kdots: Option[HasDots[A]]) extends HasDots[Set[A]] {
    extension (dotted: Set[A])
      def dots: Dots = kdots.iterator.flatMap(kd => dotted.map(kd.dots)).foldLeft(Dots.empty)(_ `union` _)
      def removeDots(dots: Dots): Option[Set[A]] =
        val res = dotted.iterator.flatMap: elem =>
          kdots.map(kd => kd.removeDots(elem)(dots)).getOrElse(Some(elem))
        .toSet
        if res.isEmpty then None
        else Some(res)
  }

  inline given setInstance[A]: HasDots[Set[A]] =
    val kdots = scala.compiletime.summonFrom:
      case hd: HasDots[A] => Some(hd)
      case _              => None
    new HasDotsSet(kdots)

  inline given mapInstance[K, V]: HasDots[Map[K, V]] =
    val kdots = scala.compiletime.summonFrom:
      case hd: HasDots[K] => hd
      case _              => noDots[K]
    val vdots = scala.compiletime.summonFrom:
      case hd: HasDots[V] => hd
      case _              => noDots[V]

    new MapHasDots(kdots, vdots)

  class MapHasDots[K, V](kdots: HasDots[K], vdots: HasDots[V]) extends HasDots[Map[K, V]] {
    extension (dotted: Map[K, V])
      def dots: Dots =
        val all = dotted.keysIterator.map(kdots.dots) ++ dotted.valuesIterator.map(vdots.dots)
        all.foldLeft(Dots.empty)(_ `union` _)

      def removeDots(dots: Dots): Option[Map[K, V]] =
        val res = dotted.flatMap: (k, v) =>
          val nk = kdots.removeDots(k)(dots)
          val nv = vdots.removeDots(v)(dots)
          nk.zip(nv)
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
    val hasDots =
      summonAll[Tuple.Map[pm.MirroredElemTypes, HasDots]].toIArray.map(_.asInstanceOf[HasDots[Any]])
    val bottoms: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, Bottom]]

    new ProductHasDots(pm, hasDots, bottoms)

  class ProductHasDots[T <: Product](pm: Mirror.ProductOf[T], children: IArray[HasDots[Any]], bottoms: Tuple)
      extends HasDots[T] {
    extension (a: T)
      def dots: Dots = Range(0, a.productArity).foldLeft(Dots.empty) { (c, i) =>
        c.union(children(i).dots(a.productElement(i)))
      }
      def removeDots(dots: Dots): Option[T] =
        val res = pm.fromProduct(new Product {
          def canEqual(that: Any): Boolean = false
          def productArity: Int            = children.size
          def productElement(i: Int): Any =
            children(i).removeDots(a.productElement(i))(dots).getOrElse {
              bottoms.productElement(i).asInstanceOf[Bottom[Any]].empty
            }
        })
        val allEmpty = res.productIterator.zipWithIndex.forall: (v, i) =>
          bottoms.productElement(i).asInstanceOf[Bottom[Any]].isEmpty(v)
        if allEmpty then None else Some(res)

  }
}
