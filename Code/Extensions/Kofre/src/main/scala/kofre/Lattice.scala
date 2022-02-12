package kofre

import scala.annotation.targetName
import scala.collection.immutable.HashMap
import scala.compiletime.summonAll
import scala.deriving.Mirror

/** Well, its technically a semilattice, but that is just more to type. */
trait Lattice[A]:
  /** By assumption: associative, commutative, idempotent.
    *
    * For use with Delta CRDTs, this function should be optimized for the case
    * that left >> right, i.e., that left is the current state and right the delta
    */
  def merge(left: A, right: A): A

object Lattice {
  def apply[A](implicit ev: Lattice[A]): Lattice[A] = ev
  def merge[A: Lattice](left: A, right: A): A       = apply[A].merge(left, right)

  // extension [A: Lattice](left: A)
  //  @targetName("mergeSyntax")
  //  def merge(right: A): A = Lattice.merge(left, right)

  // this seems to have better 2.13 compatibility
  implicit class Operators[A: Lattice](left: A):
    def merge(right: A): A = Lattice.merge(left, right)

  // /////////////// common instances below ///////////////

  given setLattice[A]: Lattice[Set[A]] = _ union _

  given optionLattice[A: Lattice]: Lattice[Option[A]] =
    case (None, r)          => r
    case (l, None)          => l
    case (Some(l), Some(r)) => Some(Lattice.merge[A](l, r))

  given mapLattice[K, V: Lattice]: Lattice[Map[K, V]] =
    (left, right) =>
      right.foldLeft(left) {
        case (current, (key, r)) =>
          current.updatedWith(key) {
            case Some(l) => Some(l merge r)
            case None    => Some(r)
          }
      }

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
