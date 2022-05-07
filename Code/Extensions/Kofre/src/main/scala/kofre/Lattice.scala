package kofre

import kofre.contextual.{WithContext, WithContextDecompose}

import scala.annotation.targetName
import scala.collection.immutable.HashMap
import scala.compiletime.summonAll
import scala.deriving.Mirror

/** Well, its technically a semilattice, but that is just more to type. */
trait Lattice[A] {

  /** By assumption: associative, commutative, idempotent.
    *
    * For use with Delta CRDTs, this function should be optimized for the case
    * that left >> right, i.e., that left is the current state and right the delta
    */
  def merge(left: A, right: A): A

  /** Lattice order is derived from merge, but should be overridden for efficiency */
  def lteq(left: A, right: A): Boolean = merge(left, right) == right
}

object Lattice {
  def apply[A](implicit ev: Lattice[A]): Lattice[A] = ev
  def merge[A: Lattice](left: A, right: A): A       = apply[A].merge(left, right)

  implicit class Operators[A: Lattice](left: A):
    infix def merge(right: A): A = Lattice[A].merge(left, right)

  given latticeOrder[A: Lattice]: PartialOrdering[A] with {
    override def tryCompare(x: A, y: A): Option[Int] = {
      val lr = lteq(x, y)
      val rl = lteq(y, x)
      (lr, rl) match
        case (true, true)   => Some(0)
        case (false, false) => None
        case (true, false)  => Some(-1)
        case (false, true)  => Some(1)
    }

    override def lteq(x: A, y: A): Boolean = Lattice[A].lteq(x, y)
  }

  // /////////////// common instances below ///////////////

  given setLattice[A]: Lattice[Set[A]] = _ union _

  given optionLattice[A: Lattice]: Lattice[Option[A]] =
    case (None, r)          => r
    case (l, None)          => l
    case (Some(l), Some(r)) => Some(l merge r)

  given mapLattice[K, V: Lattice]: Lattice[Map[K, V]] = (left, right) =>
    right.foldLeft(left) {
      case (current, (key, r)) =>
        current.updatedWith(key) {
          case Some(l) => Some(l merge r)
          case None    => Some(r)
        }
    }

  given functionLattice[K, V: Lattice]: Lattice[K => V] = (left, right) => k => left(k) merge right(k)

  given contextLattice[D: WithContextDecompose]: Lattice[WithContext[D]] = (left, right) =>
    val dsMerged = WithContextDecompose[D].mergePartial(left, right)
    val ccMerged = left.context merge right.context
    WithContext[D](dsMerged, ccMerged)

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): Lattice[T] =
    val lattices = summonAll[Tuple.Map[pm.MirroredElemTypes, Lattice]].toIArray.map(_.asInstanceOf[Lattice[Any]])
    ProductLattice(pm, lattices)

  class ProductLattice[T <: Product](m: Mirror.ProductOf[T], lattices: Seq[Lattice[Any]]) extends Lattice[T]:
    override def merge(left: T, right: T): T =
      m.fromProduct(new Product {
        def canEqual(that: Any): Boolean = false
        def productArity: Int            = lattices.length
        def productElement(i: Int): Any  = lattices(i).merge(left.productElement(i), right.productElement(i))
      })
}
