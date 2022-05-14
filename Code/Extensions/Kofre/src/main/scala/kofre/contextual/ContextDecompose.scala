package kofre.contextual

import kofre.base.Lattice.Operators
import kofre.base.{Bottom, DecomposeLattice, Lattice}
import kofre.causality.{CausalContext, Dot}
import kofre.contextual.{AsCausalContext, ContextDecompose, ContextLattice, WithContext}
import kofre.decompose.interfaces

import scala.annotation.implicitNotFound
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror
import scala.compiletime.summonAll

/** DecomposableDotStore is the typeclass trait for dot stores,
  * data structures that are part of causal CRDTs and make use of dots to track causality.
  */
@implicitNotFound("Not a decompose lattice when in a context: »${A}«")
trait ContextDecompose[A] extends ContextLattice[A], DecomposeLattice[WithContext[A]] {
  def contextbimap[B](to: WithContext[A] => WithContext[B], from: WithContext[B] => WithContext[A]): ContextDecompose[B] = new ContextDecompose[B] {
    override def lteq(left: WithContext[B], right: WithContext[B]): Boolean = ContextDecompose.this.lteq(from(left), from(right))
    override def decompose(a: WithContext[B]): Iterable[WithContext[B]] = ContextDecompose.this.decompose(from(a)).map(to)
    override def mergePartial(left: WithContext[B], right: WithContext[B]): B = to(WithContext(ContextDecompose.this.mergePartial(from(left), from(right)))).store
    override def empty: WithContext[B] = to(ContextDecompose.this.empty)
  }
}

object ContextDecompose {
  def apply[A](implicit ds: ContextDecompose[A]): ContextDecompose[A] = ds

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): ContextDecompose[T] = {
    val lattices: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, ContextDecompose]]
    new ProductContextDecompose[T](lattices, pm)
  }

  class ProductContextDecompose[T <: Product](lattices: Tuple, pm: Mirror.ProductOf[T])
      extends ContextDecompose[T] {

    private def lat(i: Int): ContextDecompose[Any] = lattices.productElement(i).asInstanceOf[ContextDecompose[Any]]

    override def mergePartial(left: WithContext[T], right: WithContext[T]): T =
      pm.fromProduct(new Product {
        def canEqual(that: Any): Boolean = false
        def productArity: Int            = lattices.productArity
        def productElement(i: Int): Any =
          lat(i).mergePartial(left.map(_.productElement(i)), right.map(_.productElement(i)))
      })

    override def empty: WithContext[T] =
      WithContext(pm.fromProduct(
        lattices.map[[α] =>> Any](
          [t] => (l: t) => l.asInstanceOf[ContextDecompose[Any]].empty.store
        )
      ))

    override def decompose(a: WithContext[T]): Iterable[WithContext[T]] =
      Range(0, lattices.productArity).flatMap { j =>
        lat(j).decompose(a.map(_.productElement(j))).map {
          _.map { elem =>
            pm.fromProduct(new Product {
              def canEqual(that: Any): Boolean = false
              def productArity: Int            = lattices.productArity
              def productElement(i: Int): Any  = if i == j then elem else lat(i).empty.store
            })
          }
        }
      }

    override def lteq(left: WithContext[T], right: WithContext[T]): Boolean =
      Range(0, lattices.productArity).forall { i =>
        lat(i).lteq(left.map(_.productElement(i)), right.map(_.productElement(i)))
      }
  }

  abstract class FromConlattice[A](wcm: ContextLattice[A]) extends ContextDecompose[A] {
    export wcm.mergePartial
  }

  /** DotLess is a dot store implementation that, in combination with [[DotPair]], allows to compose non-causal CRDTs
    * with causal CRDTs. For a usage example, see [[interfaces.RGA]], where the implicit presence of DotLess is
    * necessary so that the non-causal [[interfaces.EpocheInterface]] can be part of the [[DotPair]] that makes up
    * the state.
    *
    * Note, implementing this marks the type as independent of its context,
    * beware types with different possible interpretations.
    */
  def liftDecomposeLattice[A: DecomposeLattice]: ContextDecompose[A] =
    new ContextDecompose[A] {
      override def mergePartial(left: WithContext[A], right: WithContext[A]): A =
        Lattice[A].merge(left.store, right.store)

      override def lteq(left: WithContext[A], right: WithContext[A]): Boolean =
        DecomposeLattice[A].lteq(left.store, right.store)

      override def decompose(state: WithContext[A]): Iterable[WithContext[A]] = {
        DecomposeLattice[A].decompose(state.store).map(WithContext(_, CausalContext.empty))
      }

      override def empty: WithContext[A] = WithContext(DecomposeLattice[A].empty)
    }
}
