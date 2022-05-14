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
trait ContextDecompose[A] extends ContextLattice[A], DecomposeLattice[WithContext[A]]

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

  /** DotSet is a dot store implementation that is simply a set of dots. See [[interfaces.EnableWinsFlag]] for a
    * usage example.
    */
  implicit def DotSet: ContextDecompose[CausalContext] =
    new FromConlattice[CausalContext](ContextLattice.causalContext) {

      override def empty: WithContext[CausalContext] = WithContext(CausalContext.empty)

      override def lteq(left: WithContext[CausalContext], right: WithContext[CausalContext]): Boolean = {
        val firstCondition = left.context.forall(right.context.contains)

        val secondCondition = {
          val diff = left.context.diff(left.store)
          right.store.intersect(diff).isEmpty
        }

        firstCondition && secondCondition
      }

      override def decompose(state: WithContext[CausalContext]): Iterable[WithContext[CausalContext]] = {
        val added =
          for (d <- state.store.iterator) yield
            val single = CausalContext.single(d)
            WithContext(single, single)
        val removed = state.context.subtract(state.store).decomposed.map(WithContext(CausalContext.empty, _))
        removed ++ added
      }
    }

  /** DotMap is a dot store implementation that maps keys of an arbitrary type K to values of a dot store type V. See
    * [[interfaces.ORMapInterface]] for a usage example.
    */
  implicit def DotMap[K, V: ContextDecompose: AsCausalContext]: ContextDecompose[Map[K, V]] =
    new FromConlattice[Map[K, V]](ContextLattice.dotMapLattice) {

      override def empty: WithContext[Map[K, V]] = WithContext(Map.empty)

      override def lteq(left: WithContext[Map[K, V]], right: WithContext[Map[K, V]]): Boolean = {
        def firstCondition = (left.context subtract right.context).isEmpty

        def secondConditionHelper(keys: Iterable[K]): Boolean = keys.forall { k =>
          left.map(_.getOrElse(k, Bottom.empty[V])) <= right.map(_.getOrElse(k, Bottom.empty[V]))
        }

        def secondCondition = secondConditionHelper(left.store.keys) && secondConditionHelper(right.store.keys)

        firstCondition && secondCondition
      }

      override def decompose(state: WithContext[Map[K, V]]): Iterable[WithContext[Map[K, V]]] = {
        val added = for {
          k <- state.store.keys
          WithContext(atomicV, atomicCC) <- {
            val v = state.store.getOrElse(k, Bottom.empty[V])
            ContextDecompose[V].decompose(WithContext(v, AsCausalContext[V].dots(v)))
          }
        } yield WithContext(Map.empty[K, V].updated(k, atomicV), atomicCC)

        val removed =
          state.context.subtract(AsCausalContext.DotMapInstance.dots(state.store)).decomposed.map(WithContext(
            Map.empty[K, V],
            _
          ))

        added ++ removed
      }
    }

  /** DotPair is a dot store implementation that allows the composition of two dot stores in a pair. See [[interfaces.RGA]]
    * for a usage example
    */
  implicit def DotPair[A: ContextDecompose: AsCausalContext, B: ContextDecompose: AsCausalContext]
      : ContextDecompose[(A, B)] =
    new FromConlattice[(A, B)](ContextLattice.pairPartialLattice) {

      override def empty: WithContext[(A, B)] = WithContext((Bottom[A].empty, Bottom[B].empty))

      override def lteq(left: WithContext[(A, B)], right: WithContext[(A, B)]): Boolean =
        (left, right) match {
          case (WithContext((left1, left2), leftCContext), WithContext((right1, right2), rightCContext)) =>
            ContextDecompose[A].lteq(WithContext(left1, leftCContext), WithContext(right1, rightCContext)) &&
            ContextDecompose[B].lteq(WithContext(left2, leftCContext), WithContext(right2, rightCContext))
        }

      override def decompose(state: WithContext[(A, B)]): Iterable[WithContext[(A, B)]] = state match {
        case WithContext((state1, state2), cc) =>
          val decomposed1 = ContextDecompose[A].decompose(WithContext(state1, cc)).map {
            case WithContext(atomicState, atomicCC) =>
              WithContext((atomicState, Bottom.empty[B]), atomicCC)
          }

          val decomposed2 = ContextDecompose[B].decompose(WithContext(state2, cc)).map {
            case WithContext(atomicState, atomicCC) =>
              WithContext((Bottom.empty[A], atomicState), atomicCC)
          }

          decomposed1 ++ decomposed2
      }

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
