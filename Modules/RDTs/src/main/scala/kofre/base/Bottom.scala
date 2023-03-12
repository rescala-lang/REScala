package kofre.base

import kofre.dotted.{DotFun, DotMap, DotSet, Dotted, HasDots}
import kofre.time.Dots

import scala.collection.immutable.Queue
import scala.deriving.Mirror
import scala.compiletime.summonAll

/** Bottom.empty is the identity of Lattice.merge */
@FunctionalInterface
trait Bottom[A] {
  def empty: A
}

object Bottom {
  def empty[A](using bottom: Bottom[A]): A         = bottom.empty
  def apply[A](using bottom: Bottom[A]): Bottom[A] = bottom

  private[this] object mapBottomInstance extends Bottom[Map[Nothing, Nothing]] {
    override def empty: Map[Nothing, Nothing] = Map.empty
  }
  given mapBottom[K, V]: Bottom[Map[K, V]] = mapBottomInstance.asInstanceOf

  given optionBottom[V]: Bottom[Option[V]] with {
    override def empty: Option[V] = None
  }

  private[this] object setBottomInstance extends Bottom[Set[Nothing]] {
    override val empty: Set[Nothing] = Set.empty
  }
  given setBottom[V]: Bottom[Set[V]] = setBottomInstance.asInstanceOf

  given queueBottom[V]: Bottom[Queue[V]] with {
    override def empty: Queue[V] = Queue.empty
  }

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
      type Unbottom[A] = A match { case Bottom[b] => b }
      pm.fromProduct(
        bottoms.map([β] => (b: β) => (b match { case b: Bottom[_] => b.empty }): Unbottom[β])
      )
  }

}
