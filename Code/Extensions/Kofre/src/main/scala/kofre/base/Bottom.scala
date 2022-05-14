package kofre.base

import kofre.dotted.{DotFun, DotMap, DotSet, Dotted, DottedDecompose, HasDots}
import kofre.time.Dots

import scala.deriving.Mirror
import scala.compiletime.summonAll

/** Bottom.empty is the identity of Lattice.merge */
trait Bottom[A] {
  def empty: A
}
object Bottom {
  def empty[A](using bottom: Bottom[A]): A         = bottom.empty
  def apply[A](using bottom: Bottom[A]): Bottom[A] = bottom

  given mapBottom[K, V]: Bottom[Map[K, V]] with {
    override def empty: Map[K, V] = Map.empty
  }

  given setBottom[V]: Bottom[Set[V]] with {
    override def empty: Set[V] = Set.empty
  }

  given intMaxBottom: Bottom[Int] with {override def empty: Int = Int.MinValue}

  given dotMap[K, V]: Bottom[DotMap[K, V]]   = Bottom.derived
  given dotFun[V]: Bottom[DotFun[V]]         = Bottom.derived
  given dotSet[K, V]: Bottom[DotSet]         = Bottom.derived
  given dots: Bottom[Dots]                   = Bottom.derived
  given dotted[A: Bottom]: Bottom[Dotted[A]] = Bottom.derived

  given pairBottom[A: Bottom, B: Bottom]: Bottom[(A, B)] = Bottom.derived

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): Bottom[T] =
    val lattices = summonAll[Tuple.Map[pm.MirroredElemTypes, Bottom]]
    ProductBottom(pm, lattices)

  class ProductBottom[T <: Product](pm: Mirror.ProductOf[T], bottoms: Tuple) extends Bottom[T] {
    override def empty: T =
      pm.fromProduct(
        bottoms.map[[α] =>> Any](
          [t] => (l: t) => l.asInstanceOf[Bottom[Any]].empty
        )
      )
  }

}
