package kofre.contextual

import kofre.base.Lattice.Operators
import kofre.base.{Bottom, Decompose, DecomposeLattice, Lattice}
import kofre.causality.{CausalContext, Dot}
import kofre.contextual.{AsCausalContext, ContextDecompose, ContextLattice, WithContext}
import kofre.decompose.interfaces

import scala.annotation.implicitNotFound
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

/** DecomposableDotStore is the typeclass trait for dot stores,
  * data structures that are part of causal CRDTs and make use of dots to track causality.
  */
trait ContextDecompose[A] extends ContextLattice[A], Decompose[WithContext[A]], AsCausalContext[A] {
  def lteq(left: WithContext[A], right: WithContext[A]): Boolean
}

object ContextDecompose {
  def apply[A](implicit ds: ContextDecompose[A]): ContextDecompose[A] = ds

  def product1ContextDecompose[P <: Product, A](using
      pm: Mirror.Product { type MirroredType = P; type MirroredMonoType = P; type MirroredElemTypes = Tuple1[A] },
      innerCD: ContextDecompose[A]
  ): ContextDecompose[P] = new {
    private def access(a: P): A            = Tuple.fromProductTyped(a)._1
    override def dots(a: P): CausalContext = innerCD.dots(access(a))
    override def empty: P                  = pm.fromProduct(Tuple1(innerCD.empty))
    override def decompose(a: WithContext[P]): Iterable[WithContext[P]] =
      innerCD.decompose(a.map(access)).map(_.map(v => pm.fromProduct(Tuple1(v))))
    override def mergePartial(left: WithContext[P], right: WithContext[P]): P =
      pm.fromProduct(Tuple1(innerCD.mergePartial(left.map(access), right.map(access))))
    override def lteq(left: WithContext[P], right: WithContext[P]): Boolean =
      innerCD.lteq(left.map(access), right.map(access))
  }

  abstract class ComposeFrom[A](acc: AsCausalContext[A], wcm: ContextLattice[A]) extends ContextDecompose[A] {
    export acc.*
    export wcm.mergePartial
  }

  /** DotSet is a dot store implementation that is simply a set of dots. See [[interfaces.EnableWinsFlag]] for a
    * usage example.
    */
  implicit def DotSet: ContextDecompose[CausalContext] =
    new ComposeFrom[CausalContext](AsCausalContext.causalContextInstance, ContextLattice.causalContext) {

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
  implicit def DotMap[K, V: ContextDecompose]: ContextDecompose[Map[K, V]] =
    new ComposeFrom[Map[K, V]](AsCausalContext.DotMapInstance, ContextLattice.dotMapLattice) {

      override def lteq(left: WithContext[Map[K, V]], right: WithContext[Map[K, V]]): Boolean = {
        val firstCondition = left.context.forall(right.context.contains)

        def secondConditionHelper(keys: Iterable[K]): Boolean = keys.forall { k =>
          val leftV  = left.store.getOrElse(k, Bottom.empty[V])
          val rightV = right.store.getOrElse(k, Bottom.empty[V])

          ContextDecompose[V].lteq(WithContext(leftV, left.context), WithContext(rightV, right.context))
        }

        val secondCondition = secondConditionHelper(left.store.keys) && secondConditionHelper(right.store.keys)

        firstCondition && secondCondition
      }

      override def decompose(state: WithContext[Map[K, V]]): Iterable[WithContext[Map[K, V]]] = {
        val added = for {
          k <- state.store.keys
          WithContext(atomicV, atomicCC) <- {
            val v = state.store.getOrElse(k, Bottom.empty[V])
            ContextDecompose[V].decompose(WithContext(v, ContextDecompose[V].dots(v)))
          }
        } yield WithContext(Map.empty[K, V].updated(k, atomicV), atomicCC)

        val removed =
          state.context.subtract(dots(state.store)).decomposed.map(WithContext(Map.empty[K, V], _))

        added ++ removed
      }
    }

  /** DotPair is a dot store implementation that allows the composition of two dot stores in a pair. See [[interfaces.RGAInterface]]
    * for a usage example
    */
  implicit def DotPair[A: ContextDecompose, B: ContextDecompose]: ContextDecompose[(A, B)] =
    new ComposeFrom[(A, B)](AsCausalContext.DotPairInstance, ContextLattice.pairPartialLattice) {

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

  /** DotFun is a dot store implementation that maps dots to values of a Lattice type. See [[interfaces.MVRegisterInterface]]
    * for a usage example.
    */
  implicit def DotFun[A: DecomposeLattice]: ContextDecompose[Map[Dot, A]] =
    new ComposeFrom[Map[Dot, A]](AsCausalContext.dotFunDotStore, ContextLattice.perDot[A]) {

      override def lteq(left: WithContext[Map[Dot, A]], right: WithContext[Map[Dot, A]]): Boolean = {
        val firstCondition = left.context.forall(right.context.contains)
        val secondCondition = right.store.keySet.forall { k =>
          left.store.get(k).forall { l => DecomposeLattice[A].lteq(l, right.store(k)) }
        }
        val thirdCondition = {
          val diff = left.context.diff(DotFun[A].dots(left.store))
          DotFun[A].dots(right.store).intersect(diff).isEmpty
        }

        firstCondition && secondCondition && thirdCondition
      }

      override def decompose(state: WithContext[Map[Dot, A]]): Iterable[WithContext[Map[Dot, A]]] = {
        val added = for (d <- DotFun[A].dots(state.store).iterator; v <- DecomposeLattice[A].decompose(state.store(d)))
          yield WithContext(Map(d -> v), CausalContext.single(d))

        val removed =
          state.context.subtract(DotFun[A].dots(state.store)).decomposed.map(WithContext(DotFun[A].empty, _))

        removed ++ added
      }
    }

  /** DotLess is a dot store implementation that, in combination with [[DotPair]], allows to compose non-causal CRDTs
    * with causal CRDTs. For a usage example, see [[interfaces.RGAInterface]], where the implicit presence of DotLess is
    * necessary so that the non-causal [[interfaces.EpocheInterface]] can be part of the [[DotPair]] that makes up
    * the state.
    */
  def UIJDLatticeAsDecomposableDotStore[A: DecomposeLattice]: ContextDecompose[A] =
    new ContextDecompose[A] {
      override def dots(ds: A): CausalContext = CausalContext.empty

      override def mergePartial(left: WithContext[A], right: WithContext[A]): A =
        Lattice[A].merge(left.store, right.store)

      override def lteq(left: WithContext[A], right: WithContext[A]): Boolean =
        DecomposeLattice[A].lteq(left.store, right.store)

      override def decompose(state: WithContext[A]): Iterable[WithContext[A]] = {
        DecomposeLattice[A].decompose(state.store).map(WithContext(_, CausalContext.empty))
      }

      override def empty: A = DecomposeLattice[A].empty
    }
}
