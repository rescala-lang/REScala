package kofre.dotted

import kofre.base.Lattice.Operators
import kofre.base.{Bottom, DecomposeLattice, Lattice}
import kofre.datatypes.RGA
import kofre.decompose.interfaces
import kofre.time.{Dot, Dots}

import scala.annotation.implicitNotFound
import scala.compiletime.{erasedValue, summonAll, summonInline}
import scala.deriving.Mirror

/** DecomposableDotStore is the typeclass trait for dot stores,
  * data structures that are part of causal CRDTs and make use of dots to track time.
  */
@implicitNotFound("Not a decompose lattice when in a context: »${A}«")
trait DottedDecompose[A] extends DottedLattice[A], DecomposeLattice[Dotted[A]] {
  def contextbimap[B](to: Dotted[A] => Dotted[B], from: Dotted[B] => Dotted[A]): DottedDecompose[B] =
    new DottedDecompose[B] {
      override def lteq(left: Dotted[B], right: Dotted[B]): Boolean = DottedDecompose.this.lteq(from(left), from(right))
      override def decompose(a: Dotted[B]): Iterable[Dotted[B]]     = DottedDecompose.this.decompose(from(a)).map(to)
      override def mergePartial(left: Dotted[B], right: Dotted[B]): B =
        to(Dotted(DottedDecompose.this.mergePartial(from(left), from(right)))).store
    }
}

object DottedDecompose {
  def apply[A](implicit ds: DottedDecompose[A]): DottedDecompose[A] = ds

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): DottedDecompose[T] = {
    val lattices: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, DottedDecompose]]
    val bottoms: Tuple  = summonAll[Tuple.Map[pm.MirroredElemTypes, Bottom]]
    new ProductDottedDecompose[T](lattices, bottoms, pm, valueOf[pm.MirroredLabel])
  }

  class ProductDottedDecompose[T <: Product](lattices: Tuple, bottoms: Tuple, pm: Mirror.ProductOf[T], label: String)
      extends DottedDecompose[T] {

    override def toString: String = s"ProductDecomposeLattice[${label}]"

    private def lat(i: Int): DottedDecompose[Any] = lattices.productElement(i).asInstanceOf[DottedDecompose[Any]]
    private def bot(i: Int): Bottom[Any]          = bottoms.productElement(i).asInstanceOf[Bottom[Any]]

    override def mergePartial(left: Dotted[T], right: Dotted[T]): T =
      pm.fromProduct(new Product {
        def canEqual(that: Any): Boolean = false
        def productArity: Int            = lattices.productArity
        def productElement(i: Int): Any =
          lat(i).mergePartial(left.map(_.productElement(i)), right.map(_.productElement(i)))
      })

    override def decompose(a: Dotted[T]): Iterable[Dotted[T]] = List(a)
    // // TODO: figure out what decompose means for dotted values
    // Range(0, lattices.productArity).flatMap { j =>
    //  lat(j).decompose(a.map(_.productElement(j))).map {
    //    _.map { elem =>
    //      pm.fromProduct(new Product {
    //        def canEqual(that: Any): Boolean = false
    //        def productArity: Int            = lattices.productArity
    //        def productElement(i: Int): Any  = if i == j then elem else bot(i).empty
    //      })
    //    }
    //  }
    // }

    override def lteq(left: Dotted[T], right: Dotted[T]): Boolean =
      Range(0, lattices.productArity).forall { i =>
        lat(i).lteq(left.map(_.productElement(i)), right.map(_.productElement(i)))
      }
  }

  abstract class FromConlattice[A](wcm: DottedLattice[A]) extends DottedDecompose[A] {
    export wcm.mergePartial
  }

  /** Enables the use of a [[kofre.base.DecomposeLattice]] as a [[DottedDecompose]].
    * Beware that this works for most datastructures due to automatic derivation of the required instance,
    * but will likely not have the inteded semantics if the datastructure does use any dots inside.
    */
  def liftDecomposeLattice[A: DecomposeLattice]: DottedDecompose[A] =
    new DottedDecompose[A] {
      override def mergePartial(left: Dotted[A], right: Dotted[A]): A =
        Lattice[A].merge(left.store, right.store)

      override def lteq(left: Dotted[A], right: Dotted[A]): Boolean =
        DecomposeLattice[A].lteq(left.store, right.store)

      override def decompose(state: Dotted[A]): Iterable[Dotted[A]] = {
        DecomposeLattice[A].decompose(state.store).map(Dotted(_, Dots.empty))
      }
    }
}
