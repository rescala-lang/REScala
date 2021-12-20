package kofre

import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.*

trait Lattice[A]:
  def merge(left: A, right: A): A

extension[A] (left: A)(using l: Lattice[A])
  def merge(right: A): A = l.merge(left, right)


object Lattice:
  def merge[A](using l: Lattice[A]) = l.merge

  inline given derived[T](using m: Mirror.Of[T], c: Manifest[T]): Lattice[T] =
    lazy val elemInstances = summonAll[m.MirroredElemTypes]
    inline m match
      case p: Mirror.ProductOf[T] => mergeProduct(p, c, elemInstances)


  def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  def mergeProduct[T](p: Mirror.ProductOf[T], c: Manifest[T], lattices: => List[Lattice[_]]): Lattice[T] =
    val cons = c.runtimeClass.getDeclaredConstructors().head
    cons.setAccessible(true)

    new Lattice[T] :
      def merge(left: T, right: T): T =
        val args = iterator(left).zip(iterator(right)).zip(lattices.iterator).map {
          case ((l, r), elem) => elem.asInstanceOf[Lattice[Any]].merge(l, r)
        }.toSeq
        cons.newInstance(args.toSeq: _*).asInstanceOf[T]


inline def summonAll[T <: Tuple]: List[Lattice[_]] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => summonInline[Lattice[t]] :: summonAll[ts]


case class B(a: Int, b: Int) derives Lattice

case class D(a: B, c: B) derives Lattice

@main
def test3() =
  println((B(4, 5) merge B(6, 2)))
