package kofre

import scala.annotation.targetName
import scala.collection.immutable.HashMap
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.*
import kofre.Lattice.merge

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

  extension [A: Lattice](left: A)
    @targetName("mergeSyntax")
    def merge(right: A): A = Lattice.merge(left, right)

  ///////////////// common instances below ///////////////

  given setLattice[A]: Lattice[Set[A]] = _ union _

  given optionLattice[A: Lattice]: Lattice[Option[A]] =
    case (None, r)          => r
    case (l, None)          => l
    case (Some(l), Some(r)) => Some(Lattice.merge[A](l, r))

  given mapLattice[K, V: Lattice]: Lattice[Map[K, V]] =
    (left, right) =>
      left.to(HashMap).merged(right.to(HashMap)) {
        case ((id, v1), (_, v2)) => (id, (v1 merge v2))
      }

  inline def derived[T](using m: Mirror.Of[T], c: Manifest[T]): Lattice[T] =
    lazy val elemInstances = LatticeDeriveImpl.summonAll[m.MirroredElemTypes]
    inline m match
      case p: Mirror.ProductOf[T] => LatticeDeriveImpl.mergeProduct(p, c, elemInstances)
}


object LatticeDeriveImpl {
  def iterator[T](p: T): Iterator[Any] = p.asInstanceOf[Product].productIterator

  def mergeProduct[T](p: Mirror.ProductOf[T], c: Manifest[T], lattices: => List[Lattice[_]]): Lattice[T] =

    val cons = c.runtimeClass.getDeclaredConstructors.head
    cons.setAccessible(true)

    new Lattice[T]:
      def merge(left: T, right: T): T =
        val args = iterator(left).zip(iterator(right)).zip(lattices.iterator).map {
          case ((l, r), elem) => elem.asInstanceOf[Lattice[Any]].merge(l, r)
        }.toSeq
        cons.newInstance(args.toSeq: _*).asInstanceOf[T]

  inline def summonAll[T <: Tuple]: List[Lattice[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[Lattice[t]] :: summonAll[ts]
}
