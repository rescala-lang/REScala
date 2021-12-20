package kofre

import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.*

/** Well, its technically a semilattice, but that is just more to type. */
trait Lattice[A]:
  /** By assumption: associative, commutative, idempotent.
    *
    * For use with Delta CRDTs, this function should be optimized for the case that left >> right.
    */
  def merge(left: A, right: A): A

extension [A](left: A)(using l: Lattice[A]) def merge(right: A): A = l.merge(left, right)

object Lattice:
  def apply[A](implicit ev: Lattice[A]): Lattice[A] = ev
  def merge[A: Lattice](left: A, right: A): A       = apply[A].merge(left, right)

  implicit def setInstance[A]: Lattice[Set[A]] =
    new Lattice[Set[A]] {
      override def merge(left: Set[A], right: Set[A]): Set[A] = left.union(right)
    }

  implicit def optionLattice[A: Lattice]: Lattice[Option[A]] =
    new Lattice[Option[A]] {
      override def merge(left: Option[A], right: Option[A]): Option[A] =
        (left, right) match {
          case (None, r)          => r
          case (l, None)          => l
          case (Some(l), Some(r)) => Some(Lattice.merge[A](l, r))
        }
    }

  implicit def mapLattice[K, V: Lattice]: Lattice[Map[K, V]] =
    new Lattice[Map[K, V]] {
      override def merge(left: Map[K, V], right: Map[K, V]): Map[K, V] =
        (left.keysIterator ++ right.keysIterator)
          .toSet[K].iterator
          .flatMap { key =>
            Lattice.merge(left.get(key), right.get(key)).map(key -> _)
          }.toMap
    }

end Lattice

object LatticeDerivation {
  def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  inline def derived[T](using m: Mirror.Of[T], c: Manifest[T]): Lattice[T] =
    lazy val elemInstances = LatticeDerivation.summonAll[m.MirroredElemTypes]
    inline m match
      case p: Mirror.ProductOf[T] => LatticeDerivation.mergeProduct(p, c, elemInstances)

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
