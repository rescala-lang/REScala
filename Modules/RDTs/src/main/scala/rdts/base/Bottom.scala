package rdts.base

import rdts.dotted.Dotted
import rdts.time.Dots

import scala.collection.immutable.Queue
import scala.compiletime.{summonAll, summonInline}
import scala.deriving.Mirror

/** Provides an [[empty]] value of type [[A]]
  *
  * By assumption [[empty]] is the identity of [[Lattice.merge]]
  *
  * That is:
  * ```scala
  * Lattice.merge(empty, x) == Lattice.normalize(x)
  * ```
  */
@FunctionalInterface
trait Bottom[A] {
  def empty: A

  /** Tests if the state is an identity of [[Lattice.merge]], i.e., forall `a` with `isEmpty(a)` we require that `a merge b == b`.
    * See [[Bottom]] for cases when an empty element can be generated.
    */
  extension (value: A) def isEmpty: Boolean = value == empty
}

object Bottom {
  def provide[A](v: A) = new Bottom[A]:
    override val empty: A = v

  def empty[A](using bottom: Bottom[A]): A         = bottom.empty
  def apply[A](using bottom: Bottom[A]): Bottom[A] = bottom

  private object mapBottomInstance extends Bottom[Map[Nothing, Nothing]] {
    override def empty: Map[Nothing, Nothing]                              = Map.empty
    extension (value: Map[Nothing, Nothing]) override def isEmpty: Boolean = value.isEmpty
  }
  given mapBottom[K, V]: Bottom[Map[K, V]] = mapBottomInstance.asInstanceOf

  given optionBottom[V]: Bottom[Option[V]] with {
    override def empty: Option[V]                              = None
    extension (value: Option[V]) override def isEmpty: Boolean = value.isEmpty
  }

  private object setBottomInstance extends Bottom[Set[Nothing]] {
    override val empty: Set[Nothing]                              = Set.empty
    extension (value: Set[Nothing]) override def isEmpty: Boolean = value.isEmpty

  }
  given setBottom[V]: Bottom[Set[V]] = setBottomInstance.asInstanceOf

  given queueBottom[V]: Bottom[Queue[V]] with {
    override def empty: Queue[V]                              = Queue.empty
    extension (value: Queue[V]) override def isEmpty: Boolean = value.isEmpty

  }

  given dots: Bottom[Dots]                   = Bottom.derived
  given dotted[A: Bottom]: Bottom[Dotted[A]] = Bottom.derived

  given pairBottom[A: Bottom, B: Bottom]: Bottom[(A, B)] = Bottom.derived

  inline def derived[T](using m: Mirror.Of[T]): Bottom[T] =
    inline m match
      case pm: Mirror.ProductOf[T] => productBottom[T](using pm).asInstanceOf[Bottom[T]]
      case sm: Mirror.SumOf[T]     => sumBottom[T](using sm)

  inline def sumBottom[T](using sm: Mirror.SumOf[T]): Bottom[T] =
    val bottoms: Bottom[Derived.Head[sm.MirroredElemTypes]] = summonInline
    Derived.SumBottom[T](sm, bottoms)

  inline def productBottom[T](using pm: Mirror.ProductOf[T]): Bottom[T] =
    val lattices = summonAll[Tuple.Map[pm.MirroredElemTypes, Bottom]]
    Derived.ProductBottom(pm, lattices)

  object Derived {

    type Head[X <: Tuple] = X match {
      case x *: _ => x
    }

    class ProductBottom[T](pm: Mirror.ProductOf[T], bottoms: Tuple) extends Bottom[T] {
      override def empty: T =
        pm.fromProduct(
          Tuple.fromArray:
            bottoms.toArray.map[AnyRef](_.asInstanceOf[Bottom[AnyRef]].empty)
        )
      extension (value: T)
        override def isEmpty: Boolean =
          value.asInstanceOf[Product].productIterator.zipWithIndex.forall: (v, i) =>
            bottoms.productElement(i).asInstanceOf[Bottom[Any]].isEmpty(v)
    }

    class SumBottom[T](sm: Mirror.SumOf[T], bottoms: Bottom[Head[sm.MirroredElemTypes]])
        extends Bottom[T] {
      override def empty: T = bottoms.empty.asInstanceOf[T]
      extension (value: T)
        override def isEmpty: Boolean =
          sm.ordinal(value) == 0 && bottoms.isEmpty(value.asInstanceOf[Head[sm.MirroredElemTypes]])
    }

  }
}

case class BottomOpt[A](maybeBottom: Option[Bottom[A]]):
  inline def withBottom[R](inline block: Bottom[A] ?=> R): Option[R] = maybeBottom match
    case Some(value) => Some(block(using value))
    case None        => None

object BottomOpt:
  inline def explicit[A, R](using inline bo: BottomOpt[A])(inline block: Bottom[A] => R): Option[R] =
    bo.withBottom(bo ?=> block(bo))

  inline given bottomOpt[A]: BottomOpt[A] =
    BottomOpt:
      scala.compiletime.summonFrom:
        case b: Bottom[A] => Some(b)
        case _            => None
