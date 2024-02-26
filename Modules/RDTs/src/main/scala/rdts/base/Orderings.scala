package rdts.base

import scala.annotation.targetName
import scala.compiletime.{erasedValue, summonAll, summonFrom, summonInline}
import scala.deriving.Mirror

object Orderings {

  inline def lexicographic[T <: Product](using pm: Mirror.ProductOf[T]): Ordering[T] = {
    val orderings: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, Ordering]]
    new LexicographicOrdering[T](orderings, pm)
  }

  class LexicographicOrdering[T <: Product](
      orderings: Tuple,
      pm: Mirror.ProductOf[T],
  ) extends Ordering[T] {
    override def compare(x: T, y: T): Int =
      def rec(idx: Int): Int =
        if idx >= orderings.productArity
        then 0
        else
          orderings.productElement(idx).asInstanceOf[Ordering[Any]].compare(
            x.productElement(idx),
            y.productElement(idx)
          ) match
            case 0            => rec(idx + 1)
            case lt if lt < 0 => -1
            case gt if gt > 0 => 1
      rec(0)

  }

}
