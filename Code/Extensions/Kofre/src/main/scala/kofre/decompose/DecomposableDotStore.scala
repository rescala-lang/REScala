package kofre.decompose

import kofre.Lattice
import kofre.causality.{CausalContext, Dot}
import kofre.dotbased.{AsCausalContext, WithContext, WithContextMerge}
import kofre.Lattice.Operators


/** DecomposableDotStore is the typeclass trait for dot stores, data structures that are part of causal CRDTs and make use of dots to
  * track causality.
  */
trait DecomposableDotStore[A] extends WithContextMerge[A], Decompose[WithContext[A]] {

  def dots(state: A): CausalContext

  def lteq(left: WithContext[A], right: WithContext[A]): Boolean

  def decompose(state: WithContext[A]): Iterable[WithContext[A]]

  def empty: A

}

object DecomposableDotStore {
  def apply[A](implicit ds: DecomposableDotStore[A]): DecomposableDotStore[A] = ds

  /** DotSet is a dot store implementation that is simply a set of dots. See [[interfaces.EWFlagInterface]] for a
    * usage example.
    */
  implicit def DotSet: DecomposableDotStore[CausalContext] = new DecomposableDotStore[CausalContext] {
    override def dots(ds: CausalContext): CausalContext = ds

    override def mergePartial(left: WithContext[CausalContext], right: WithContext[CausalContext]): CausalContext = {
      val fromLeft  = left.store subtract right.context
      val fromRight = right.store.subtract(left.context subtract left.store )

      fromLeft union fromRight
    }

    override def empty: CausalContext = CausalContext.empty

    override def lteq(left: WithContext[CausalContext], right: WithContext[CausalContext]): Boolean = {
      val firstCondition = left.context.forall(right.context.contains)

      val secondCondition = {
        val diff = left.context.diff(left.store)
        right.store.intersect(diff).isEmpty
      }

      firstCondition && secondCondition
    }

    override def decompose(state: WithContext[CausalContext]): Iterable[WithContext[CausalContext]] = {
      val added   = for (d <- state.store.iterator) yield
        val single = CausalContext.single(d)
        WithContext(single, single)
      val removed = state.context.decompose(state.store.contains).map(WithContext(CausalContext.empty, _))
      removed ++ added
    }
  }

  /** DotMap is a dot store implementation that maps keys of an arbitrary type K to values of a dot store type V. See
    * [[interfaces.ORMapInterface]] for a usage example.
    */
  implicit def DotMap[K, V: DecomposableDotStore]: DecomposableDotStore[Map[K, V]] = new DecomposableDotStore[Map[K, V]] {
    override def dots(ds: Map[K, V]): CausalContext = ds.values.foldLeft(CausalContext.empty)((acc, v) => acc merge DecomposableDotStore[V].dots(v))

    given AsCausalContext[V] with {
      override def dots(a:  V): CausalContext = DecomposableDotStore[V].dots(a)
      override def empty: V = DecomposableDotStore[V].empty
    }

    val withContextMerge: WithContextMerge[Map[K, V]] = WithContextMerge.dotMapMerge
    export withContextMerge.mergePartial

    override def empty: Map[K, V] = Map.empty[K, V]

    override def lteq(left: WithContext[Map[K, V]], right: WithContext[Map[K, V]]): Boolean = {
      val firstCondition = left.context.forall(right.context.contains)

      def secondConditionHelper(keys: Iterable[K]): Boolean = keys.forall { k =>
        val leftV  = left.store.getOrElse(k, DecomposableDotStore[V].empty)
        val rightV = right.store.getOrElse(k, DecomposableDotStore[V].empty)

        DecomposableDotStore[V].lteq(WithContext(leftV, left.context), WithContext(rightV, right.context))
      }

      val secondCondition = secondConditionHelper(left.store.keys) && secondConditionHelper(right.store.keys)

      firstCondition && secondCondition
    }

    override def decompose(state: WithContext[Map[K, V]]): Iterable[WithContext[Map[K, V]]] = {
      val added = for {
        k <- state.store.keys
        WithContext(atomicV, atomicCC) <- {
          val v = state.store.getOrElse(k, DecomposableDotStore[V].empty)
          DecomposableDotStore[V].decompose(WithContext(v, DecomposableDotStore[V].dots(v)))
        }
      } yield WithContext(Map.empty[K, V].updated(k, atomicV), atomicCC)

      val removed =
        state.context.decompose(DotMap[K, V].dots(state.store).contains).map(WithContext(Map.empty[K, V], _))

      added ++ removed
    }
  }

  /** DotPair is a dot store implementation that allows the composition of two dot stores in a pair. See [[interfaces.RGAInterface]]
    * for a usage example
    */
  implicit def DotPair[A: DecomposableDotStore, B: DecomposableDotStore]: DecomposableDotStore[(A, B)] = new DecomposableDotStore[(A, B)] {
    override def dots(ds: (A, B)): CausalContext = ds match {
      case (ds1, ds2) => DecomposableDotStore[A].dots(ds1) union DecomposableDotStore[B].dots(ds2)
    }

    override def mergePartial(left: WithContext[(A, B)], right: WithContext[(A, B)]): (A, B) =
      (left, right) match {
        case (WithContext((left1, left2), leftCContext), WithContext((right1, right2), rightCContext)) =>
          val stateMerged1 =
            DecomposableDotStore[A].mergePartial(WithContext(left1, leftCContext), WithContext(right1, rightCContext))
          val stateMerged2 =
            DecomposableDotStore[B].mergePartial(WithContext(left2, leftCContext), WithContext(right2, rightCContext))

          (stateMerged1, stateMerged2)
      }

    override def lteq(left: WithContext[(A, B)], right: WithContext[(A, B)]): Boolean =
      (left, right) match {
        case (WithContext((left1, left2), leftCContext), WithContext((right1, right2), rightCContext)) =>
          DecomposableDotStore[A].lteq(WithContext(left1, leftCContext), WithContext(right1, rightCContext)) &&
          DecomposableDotStore[B].lteq(WithContext(left2, leftCContext), WithContext(right2, rightCContext))
      }

    override def decompose(state: WithContext[(A, B)]): Iterable[WithContext[(A, B)]] = state match {
      case WithContext((state1, state2), cc) =>
        val decomposed1 = DecomposableDotStore[A].decompose(WithContext(state1, cc)).map {
          case WithContext(atomicState, atomicCC) =>
            WithContext((atomicState, DecomposableDotStore[B].empty), atomicCC)
        }

        val decomposed2 = DecomposableDotStore[B].decompose(WithContext(state2, cc)).map {
          case WithContext(atomicState, atomicCC) =>
            WithContext((DecomposableDotStore[A].empty, atomicState), atomicCC)
        }

        decomposed1 ++ decomposed2
    }

    override def empty: (A, B) = (DecomposableDotStore[A].empty, DecomposableDotStore[B].empty)
  }




  /** DotFun is a dot store implementation that maps dots to values of a Lattice type. See [[interfaces.MVRegisterInterface]]
    * for a usage example.
    */
  implicit def DotFun[A: UIJDLattice]: DecomposableDotStore[Map[Dot, A]] = new DecomposableDotStore[Map[Dot, A]] {
    override def dots(ds: Map[Dot, A]): CausalContext = CausalContext.fromSet(ds.keySet)

    private val wcm = WithContextMerge.perDot[A]
    export wcm.mergePartial

    override def empty: Map[Dot, A] = Map.empty[Dot, A]

    override def lteq(left: WithContext[Map[Dot, A]], right: WithContext[Map[Dot, A]]): Boolean = {
      val firstCondition = left.context.forall(right.context.contains)
      val secondCondition = right.store.keySet.forall { k =>
        left.store.get(k).forall { l => UIJDLattice[A].lteq(l, right.store(k)) }
      }
      val thirdCondition = {
        val diff = left.context.diff(DotFun[A].dots(left.store))
        DotFun[A].dots(right.store).intersect(diff).isEmpty
      }

      firstCondition && secondCondition && thirdCondition
    }

    override def decompose(state: WithContext[Map[Dot, A]]): Iterable[WithContext[Map[Dot, A]]] = {
      val added = for (d <- DotFun[A].dots(state.store).iterator; v <- UIJDLattice[A].decompose(state.store(d)))
        yield WithContext(Map(d -> v), CausalContext.single(d))

      val removed =
        state.context.decompose(DotFun[A].dots(state.store).contains).map(WithContext(DotFun[A].empty, _))

      removed ++ added
    }
  }

  /** DotLess is a dot store implementation that, in combination with [[DotPair]], allows to compose non-causal CRDTs
    * with causal CRDTs. For a usage example, see [[interfaces.RGAInterface]], where the implicit presence of DotLess is
    * necessary so that the non-causal [[interfaces.EpocheInterface]] can be part of the [[DotPair]] that makes up
    * the state.
    */
  implicit def UIJDLatticeAsDecomposableDotStore[A: UIJDLattice]: DecomposableDotStore[A] = new DecomposableDotStore[A] {
    override def dots(ds: A): CausalContext = CausalContext.empty

    override def mergePartial(left: WithContext[A], right: WithContext[A]): A =
      Lattice[A].merge(left.store, right.store)

    override def lteq(left: WithContext[A], right: WithContext[A]): Boolean =
      UIJDLattice[A].lteq(left.store, right.store)

    override def decompose(state: WithContext[A]): Iterable[WithContext[A]] = {
      UIJDLattice[A].decompose(state.store).map(WithContext(_, CausalContext.empty))
    }

    override def empty: A = UIJDLattice[A].empty
  }
}
