package kofre.decompose

import kofre.Lattice
import kofre.causality.{CausalContext, Dot}
import kofre.dotbased.CausalStore
import kofre.Lattice.Operators


/** DecomposableDotStore is the typeclass trait for dot stores, data structures that are part of causal CRDTs and make use of dots to
  * track causality.
  */
trait DecomposableDotStore[A] {

  def dots(state: A): CausalContext

  def mergePartial(left: CausalStore[A], right: CausalStore[A]): A

  def leq(left: CausalStore[A], right: CausalStore[A]): Boolean

  def decompose(state: CausalStore[A]): Iterable[CausalStore[A]]

  def empty: A
}

object DecomposableDotStore {
  def apply[A](implicit ds: DecomposableDotStore[A]): DecomposableDotStore[A] = ds

  /** DotSet is a dot store implementation that is simply a set of dots. See [[interfaces.EWFlagInterface]] for a
    * usage example.
    */
  implicit def DotSet: DecomposableDotStore[CausalContext] = new DecomposableDotStore[CausalContext] {
    override def dots(ds: CausalContext): CausalContext = ds

    override def mergePartial(left: CausalStore[CausalContext], right: CausalStore[CausalContext]): CausalContext = {
      val fromLeft  = left.store subtract right.context
      val fromRight = right.store.subtract(left.context subtract left.store )

      fromLeft union fromRight
    }

    override def empty: CausalContext = CausalContext.empty

    override def leq(left: CausalStore[CausalContext], right: CausalStore[CausalContext]): Boolean = {
      val firstCondition = left.context.forall(right.context.contains)

      val secondCondition = {
        val diff = left.context.diff(left.store)
        right.store.intersect(diff).isEmpty
      }

      firstCondition && secondCondition
    }

    override def decompose(state: CausalStore[CausalContext]): Iterable[CausalStore[CausalContext]] = {
      val added   = for (d <- state.store.iterator) yield
        val single = CausalContext.single(d)
        CausalStore(single, single)
      val removed = state.context.decompose(state.store.contains).map(CausalStore(CausalContext.empty, _))
      removed ++ added
    }
  }

  /** DotMap is a dot store implementation that maps keys of an arbitrary type K to values of a dot store type V. See
    * [[interfaces.ORMapInterface]] for a usage example.
    */
  type DotMap[K, V] = Map[K, V]
  implicit def DotMap[K, V: DecomposableDotStore]: DecomposableDotStore[DotMap[K, V]] = new DecomposableDotStore[DotMap[K, V]] {
    override def dots(ds: DotMap[K, V]): CausalContext = ds.values.foldLeft(CausalContext.empty)((acc, v) => acc merge DecomposableDotStore[V].dots(v))

    override def mergePartial(
                               left: CausalStore[DotMap[K, V]],
                               right: CausalStore[DotMap[K, V]]
                             ): DotMap[K, V] = {
      def mergeHelp(l: V, r: V): Option[V] = {
        val mergedVal = DecomposableDotStore[V].mergePartial(CausalStore(l, left.context), CausalStore(r, right.context))
        if (mergedVal == DecomposableDotStore[V].empty) None
        else Some(mergedVal)
      }

      var rightSet = right.context

      val added = right.store.foldLeft(left.store) { case (currentLeft, (k, r)) =>
        rightSet = rightSet.subtract(DecomposableDotStore[V].dots(r))
        currentLeft.updatedWith(k) {
          case None    => mergeHelp(DecomposableDotStore[V].empty, r)
          case Some(l) => mergeHelp(l, r)
        }
      }

      if (rightSet.isEmpty) added
      else {
        added.foldLeft(added) { case (current, (k, l)) =>
          if (right.store.contains(k)) current
          else mergeHelp(l, DecomposableDotStore[V].empty) match {
            case None         => current.removed(k)
            case Some(merged) => current.updated(k, merged)
          }
        }
      }
    }

    override def empty: DotMap[K, V] = Map.empty[K, V]

    override def leq(left: CausalStore[DotMap[K, V]], right: CausalStore[DotMap[K, V]]): Boolean = {
      val firstCondition = left.context.forall(right.context.contains)

      def secondConditionHelper(keys: Iterable[K]): Boolean = keys.forall { k =>
        val leftV  = left.store.getOrElse(k, DecomposableDotStore[V].empty)
        val rightV = right.store.getOrElse(k, DecomposableDotStore[V].empty)

        DecomposableDotStore[V].leq(CausalStore(leftV, left.context), CausalStore(rightV, right.context))
      }

      val secondCondition = secondConditionHelper(left.store.keys) && secondConditionHelper(right.store.keys)

      firstCondition && secondCondition
    }

    override def decompose(state: CausalStore[DotMap[K, V]]): Iterable[CausalStore[DotMap[K, V]]] = {
      val added = for {
        k <- state.store.keys
        CausalStore(atomicV, atomicCC) <- {
          val v = state.store.getOrElse(k, DecomposableDotStore[V].empty)
          DecomposableDotStore[V].decompose(CausalStore(v, DecomposableDotStore[V].dots(v)))
        }
      } yield CausalStore(DotMap[K, V].empty.updated(k, atomicV), atomicCC)

      val removed =
        state.context.decompose(DotMap[K, V].dots(state.store).contains).map(CausalStore(DotMap[K, V].empty, _))

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

    override def mergePartial(left: CausalStore[(A, B)], right: CausalStore[(A, B)]): (A, B) =
      (left, right) match {
        case (CausalStore((left1, left2), leftCContext), CausalStore((right1, right2), rightCContext)) =>
          val stateMerged1 =
            DecomposableDotStore[A].mergePartial(CausalStore(left1, leftCContext), CausalStore(right1, rightCContext))
          val stateMerged2 =
            DecomposableDotStore[B].mergePartial(CausalStore(left2, leftCContext), CausalStore(right2, rightCContext))

          (stateMerged1, stateMerged2)
      }

    override def leq(left: CausalStore[(A, B)], right: CausalStore[(A, B)]): Boolean =
      (left, right) match {
        case (CausalStore((left1, left2), leftCContext), CausalStore((right1, right2), rightCContext)) =>
          DecomposableDotStore[A].leq(CausalStore(left1, leftCContext), CausalStore(right1, rightCContext)) &&
          DecomposableDotStore[B].leq(CausalStore(left2, leftCContext), CausalStore(right2, rightCContext))
      }

    override def decompose(state: CausalStore[(A, B)]): Iterable[CausalStore[(A, B)]] = state match {
      case CausalStore((state1, state2), cc) =>
        val decomposed1 = DecomposableDotStore[A].decompose(CausalStore(state1, cc)).map {
          case CausalStore(atomicState, atomicCC) =>
            CausalStore((atomicState, DecomposableDotStore[B].empty), atomicCC)
        }

        val decomposed2 = DecomposableDotStore[B].decompose(CausalStore(state2, cc)).map {
          case CausalStore(atomicState, atomicCC) =>
            CausalStore((DecomposableDotStore[A].empty, atomicState), atomicCC)
        }

        decomposed1 ++ decomposed2
    }

    override def empty: (A, B) = (DecomposableDotStore[A].empty, DecomposableDotStore[B].empty)
  }


  /** DotFun is a dot store implementation that maps dots to values of a Lattice type. See [[interfaces.MVRegisterInterface]]
    * for a usage example.
    */
  type DotFun[A] = Map[Dot, A]
  implicit def DotFun[A: UIJDLattice]: DecomposableDotStore[Map[Dot, A]] = new DecomposableDotStore[Map[Dot, A]] {
    override def dots(ds: Map[Dot, A]): CausalContext = CausalContext.fromSet(ds.keySet)

    override def mergePartial(left: CausalStore[Map[Dot, A]], right: CausalStore[Map[Dot, A]]): Map[Dot, A] = {
      val fromLeft = left.store.filter { case (dot, _) => !right.context.contains(dot) }

      right.store.foldLeft(fromLeft) {
        case (m, (dot, r)) =>
          left.store.get(dot) match {
            case None =>
              if (left.context.contains(dot)) m
              else m.updated(dot, r)
            case Some(l) => m.updated(dot, UIJDLattice[A].merge(l, r))
          }
      }
    }

    override def empty: Map[Dot, A] = Map.empty[Dot, A]

    override def leq(left: CausalStore[Map[Dot, A]], right: CausalStore[Map[Dot, A]]): Boolean = {
      val firstCondition = left.context.forall(right.context.contains)
      val secondCondition = right.store.keySet.forall { k =>
        left.store.get(k).forall { l => UIJDLattice[A].leq(l, right.store(k)) }
      }
      val thirdCondition = {
        val diff = left.context.diff(DotFun[A].dots(left.store))
        DotFun[A].dots(right.store).intersect(diff).isEmpty
      }

      firstCondition && secondCondition && thirdCondition
    }

    override def decompose(state: CausalStore[Map[Dot, A]]): Iterable[CausalStore[Map[Dot, A]]] = {
      val added = for (d <- DotFun[A].dots(state.store).iterator; v <- UIJDLattice[A].decompose(state.store(d)))
        yield CausalStore(Map(d -> v), CausalContext.single(d))

      val removed =
        state.context.decompose(DotFun[A].dots(state.store).contains).map(CausalStore(DotFun[A].empty, _))

      removed ++ added
    }
  }

  /** DotLess is a dot store implementation that, in combination with [[DotPair]], allows to compose non-causal CRDTs
    * with causal CRDTs. For a usage example, see [[interfaces.RGAInterface]], where the implicit presence of DotLess is
    * necessary so that the non-causal [[interfaces.EpocheInterface]] can be part of the [[DotPair]] that makes up
    * the state.
    */
  type DotLess[A] = A
  implicit def DotLess[A: UIJDLattice]: DecomposableDotStore[A] = new DecomposableDotStore[A] {
    override def dots(ds: A): CausalContext = CausalContext.empty

    override def mergePartial(left: CausalStore[A], right: CausalStore[A]): A =
      Lattice[A].merge(left.store, right.store)

    override def leq(left: CausalStore[A], right: CausalStore[A]): Boolean =
      UIJDLattice[A].leq(left.store, right.store)

    override def decompose(state: CausalStore[A]): Iterable[CausalStore[A]] = {
      UIJDLattice[A].decompose(state.store).map(CausalStore(_, CausalContext.empty))
    }

    override def empty: A = UIJDLattice[A].empty
  }
}
