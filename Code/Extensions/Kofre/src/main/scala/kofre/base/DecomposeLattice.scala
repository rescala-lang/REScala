package kofre.base

import kofre.base.{Bottom, Lattice}
import kofre.dotted.{Dotted, DottedDecompose}
import kofre.time.Dots

import scala.annotation.implicitNotFound
import scala.compiletime.summonAll
import scala.deriving.Mirror

/** Decomposition tries to decompose a lattice into its smallest constituents.
  * The only requirement is that merging the decomposed results produces the original state.
  * Requires a bottom to enable automatic decomposition of Product types
  */
@implicitNotFound("Not a decompose lattice »${A}«")
trait DecomposeLattice[A] extends Lattice[A] {

  /** computes delta without state */
  def diff(state: A, delta: A): Option[A] = {
    decompose(delta).filter(!lteq(_, state)).reduceOption(merge)
  }

  /** Decompose a state into smaller parts.
    * Note that the goal here is small individual storage size at reasonable computational cost.
    * Minimalism of returned results is not guaranteed.
    * It is also not guaranteed that the result does not overlap.
    */
  def decompose(a: A): Iterable[A]

  extension (a: A) def decomposed: Iterable[A] = decompose(a)

}

object DecomposeLattice {
  def apply[A](implicit l: DecomposeLattice[A]): DecomposeLattice[A]    = l
  def decomposed[A](a: A)(implicit l: DecomposeLattice[A]): Iterable[A] = l.decomposed(a)

  /** reuse existing lattice instance to implement a DecomposeLattice */
  trait DecomposeFromLattice[A](lattice: Lattice[A]) extends DecomposeLattice[A] {
    export lattice.merge
  }

  given intMaxLattice: DecomposeLattice[Int] = new DecomposeFromLattice[Int](_ max _) {
    override def lteq(left: Int, right: Int): Boolean = left <= right
    override def decompose(state: Int): Iterable[Int] = List(state)
  }

  given setLattice[A]: DecomposeLattice[Set[A]] = new DecomposeFromLattice[Set[A]](Lattice.setLattice) {
    override def lteq(left: Set[A], right: Set[A]): Boolean = left subsetOf right
    override def decompose(state: Set[A]): Iterable[Set[A]] = state.map(Set(_))
  }

  given optionLattice[A: DecomposeLattice]: DecomposeLattice[Option[A]] =
    new DecomposeFromLattice[Option[A]](Lattice.optionLattice) {
      override def lteq(left: Option[A], right: Option[A]): Boolean = (left, right) match {
        case (None, _)          => true
        case (Some(_), None)    => false
        case (Some(l), Some(r)) => l <= r
      }

      override def decompose(state: Option[A]): Iterable[Option[A]] = state match {
        case None    => List.empty[Option[A]]
        case Some(v) => v.decomposed.map(Some(_))
      }
    }

  given mapLattice[K, V: DecomposeLattice]: DecomposeLattice[Map[K, V]] =
    new DecomposeFromLattice[Map[K, V]](Lattice.mapLattice) {
      override def lteq(left: Map[K, V], right: Map[K, V]): Boolean =
        left.keySet.forall { k =>
          left.get(k) <= right.get(k)
        }

      override def decompose(state: Map[K, V]): Iterable[Map[K, V]] = state.keys.flatMap { k =>
        DecomposeLattice[V].decompose(state(k)).map(v => Map(k -> v)) match {
          case s if s.isEmpty => List(Map(k -> state(k)))
          case s              => s
        }
      }
    }

  given PairAsUIJDLattice[A: DecomposeLattice: Bottom, B: DecomposeLattice: Bottom]: DecomposeLattice[(A, B)] = derived
  given TripleAsUIJDLattice[A: DecomposeLattice: Bottom, B: DecomposeLattice: Bottom, C: DecomposeLattice: Bottom]
      : DecomposeLattice[(A, B, C)] = derived

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): DecomposeLattice[T] = {
    val lattices: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, DecomposeLattice]]
    val bottoms: Tuple  = summonAll[Tuple.Map[pm.MirroredElemTypes, Bottom]]
    new ProductDecomposeLattice[T](lattices, bottoms, pm, valueOf[pm.MirroredLabel])
  }

  class ProductDecomposeLattice[T <: Product](lattices: Tuple, bottoms: Tuple, pm: Mirror.ProductOf[T], label: String)
      extends DecomposeLattice[T] {

    override def toString: String = s"ProductDecomposeLattice[${label}]"

    private def lat(i: Int): DecomposeLattice[Any] = lattices.productElement(i).asInstanceOf[DecomposeLattice[Any]]
    private def bot(i: Int): Bottom[Any]           = bottoms.productElement(i).asInstanceOf[Bottom[Any]]

    override def merge(left: T, right: T): T =
      pm.fromProduct(new Product {
        def canEqual(that: Any): Boolean = false
        def productArity: Int            = lattices.productArity
        def productElement(i: Int): Any  = lat(i).merge(left.productElement(i), right.productElement(i))
      })

    override def decompose(a: T): Iterable[T] =
      Range(0, lattices.productArity).flatMap { j =>
        lat(j).decompose(a.productElement(j)).map { elem =>
          pm.fromProduct(new Product {
            def canEqual(that: Any): Boolean = false
            def productArity: Int            = lattices.productArity
            def productElement(i: Int): Any  = if i == j then elem else bot(i).empty
          })
        }
      }

    override def lteq(left: T, right: T): Boolean = Range(0, lattices.productArity).forall { i =>
      lat(i).lteq(left.productElement(i), right.productElement(i))
    }
  }
}
