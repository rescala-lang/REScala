package kofre.decompose

import kofre.Lattice
import kofre.causality.{CausalContext, Dot}
import kofre.dotbased.CausalStore

/** DotStore is the typeclass trait for dot stores, data structures that are part of causal CRDTs and make use of dots to
  * track causality.
  */
trait DotStore[A] {

  // todo this should probably return a causal context
  def dots(state: A): Set[Dot]

  def mergePartial(left: CausalStore[A], right: CausalStore[A]): A

  def leq(left: CausalStore[A], right: CausalStore[A]): Boolean

  def decompose(state: CausalStore[A]): Iterable[CausalStore[A]]

  def empty: A
}

object DotStore {
  def apply[A](implicit ds: DotStore[A]): DotStore[A] = ds

  // this was generic, but no longer
  type C = CausalContext

  /** DotSet is a dot store implementation that is simply a set of dots. See [[interfaces.EWFlagInterface]] for a
    * usage example.
    */
  type DotSet = Set[Dot]
  implicit def DotSet: DotStore[Set[Dot]] = new DotStore[Set[Dot]] {
    override def dots(ds: Set[Dot]): Set[Dot] = ds

    override def mergePartial(left: CausalStore[Set[Dot]], right: CausalStore[Set[Dot]]): Set[Dot] = {
      val fromLeft  = left.store.filter(!right.context.contains(_))
      val fromRight = right.store.filter(dot => left.store.contains(dot) || !left.context.contains(dot))

      fromLeft union fromRight
    }

    override def empty: Set[Dot] = Set.empty[Dot]

    override def leq(left: CausalStore[Set[Dot]], right: CausalStore[Set[Dot]]): Boolean = {
      val firstCondition = left.context.forall(right.context.contains)

      val secondCondition = {
        val diff = left.context.diff(CausalContext.fromSet(left.store))
        !right.store.exists(diff.contains)
      }

      firstCondition && secondCondition
    }

    override def decompose(state: CausalStore[Set[Dot]]): Iterable[CausalStore[Set[Dot]]] = {
      val added   = for (d <- state.store) yield CausalStore(Set(d), CausalContext.one(d))
      val removed = state.context.decompose(state.store.contains).map(CausalStore(DotSet.empty, _))
      removed ++ added
    }
  }

  /** DotFun is a dot store implementation that maps dots to values of a Lattice type. See [[interfaces.MVRegisterInterface]]
    * for a usage example.
    */
  type DotFun[A] = Map[Dot, A]
  implicit def DotFun[A: UIJDLattice]: DotStore[Map[Dot, A]] = new DotStore[Map[Dot, A]] {
    override def dots(ds: Map[Dot, A]): Set[Dot] = ds.keySet

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
        val diff = left.context.diff(CausalContext.fromSet(DotFun[A].dots(left.store)))
        !DotFun[A].dots(right.store).exists(diff.contains)
      }

      firstCondition && secondCondition && thirdCondition
    }

    override def decompose(state: CausalStore[Map[Dot, A]]): Iterable[CausalStore[Map[Dot, A]]] = {
      val added = for (d <- DotFun[A].dots(state.store); v <- UIJDLattice[A].decompose(state.store(d)))
        yield CausalStore(Map(d -> v), CausalContext.one(d))

      val removed =
        state.context.decompose(DotFun[A].dots(state.store).contains).map(CausalStore(DotFun[A].empty, _))

      removed ++ added
    }
  }

  /** DotMap is a dot store implementation that maps keys of an arbitrary type K to values of a dot store type V. See
    * [[interfaces.ORMapInterface]] for a usage example.
    */
  type DotMap[K, V] = Map[K, V]
  implicit def DotMap[K, V: DotStore]: DotStore[DotMap[K, V]] = new DotStore[DotMap[K, V]] {
    override def dots(ds: DotMap[K, V]): Set[Dot] = ds.values.flatMap(DotStore[V].dots(_)).toSet

    override def mergePartial(
                               left: CausalStore[DotMap[K, V]],
                               right: CausalStore[DotMap[K, V]]
    ): DotMap[K, V] = {
      def mergeHelp(l: V, r: V): Option[V] = {
        val mergedVal = DotStore[V].mergePartial(CausalStore(l, left.context), CausalStore(r, right.context))
        if (mergedVal == DotStore[V].empty) None
        else Some(mergedVal)
      }

      var rightSet = right.context.toSet

      val added = right.store.foldLeft(left.store) { case (currentLeft, (k, r)) =>
        rightSet --= DotStore[V].dots(r)
        currentLeft.updatedWith(k) {
          case None    => mergeHelp(DotStore[V].empty, r)
          case Some(l) => mergeHelp(l, r)
        }
      }

      if (rightSet.isEmpty) added
      else {
        added.foldLeft(added) { case (current, (k, l)) =>
          if (right.store.contains(k)) current
          else mergeHelp(l, DotStore[V].empty) match {
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
        val leftV  = left.store.getOrElse(k, DotStore[V].empty)
        val rightV = right.store.getOrElse(k, DotStore[V].empty)

        DotStore[V].leq(CausalStore(leftV, left.context), CausalStore(rightV, right.context))
      }

      val secondCondition = secondConditionHelper(left.store.keys) && secondConditionHelper(right.store.keys)

      firstCondition && secondCondition
    }

    override def decompose(state: CausalStore[DotMap[K, V]]): Iterable[CausalStore[DotMap[K, V]]] = {
      val added = for {
        k <- state.store.keys
        CausalStore(atomicV, atomicCC) <- {
          val v = state.store.getOrElse(k, DotStore[V].empty)
          DotStore[V].decompose(CausalStore(v, CausalContext.fromSet(DotStore[V].dots(v))))
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
  implicit def DotPair[A: DotStore, B: DotStore]: DotStore[(A, B)] = new DotStore[(A, B)] {
    override def dots(ds: (A, B)): Set[Dot] = ds match {
      case (ds1, ds2) => DotStore[A].dots(ds1) union DotStore[B].dots(ds2)
    }

    override def mergePartial(left: CausalStore[(A, B)], right: CausalStore[(A, B)]): (A, B) =
      (left, right) match {
        case (CausalStore((left1, left2), leftCContext), CausalStore((right1, right2), rightCContext)) =>
          val stateMerged1 = DotStore[A].mergePartial(CausalStore(left1, leftCContext), CausalStore(right1, rightCContext))
          val stateMerged2 = DotStore[B].mergePartial(CausalStore(left2, leftCContext), CausalStore(right2, rightCContext))

          (stateMerged1, stateMerged2)
      }

    override def leq(left: CausalStore[(A, B)], right: CausalStore[(A, B)]): Boolean =
      (left, right) match {
        case (CausalStore((left1, left2), leftCContext), CausalStore((right1, right2), rightCContext)) =>
          DotStore[A].leq(CausalStore(left1, leftCContext), CausalStore(right1, rightCContext)) &&
          DotStore[B].leq(CausalStore(left2, leftCContext), CausalStore(right2, rightCContext))
      }

    override def decompose(state: CausalStore[(A, B)]): Iterable[CausalStore[(A, B)]] = state match {
      case CausalStore((state1, state2), cc) =>
        val decomposed1 = DotStore[A].decompose(CausalStore(state1, cc)).map {
          case CausalStore(atomicState, atomicCC) =>
            CausalStore((atomicState, DotStore[B].empty), atomicCC)
        }

        val decomposed2 = DotStore[B].decompose(CausalStore(state2, cc)).map {
          case CausalStore(atomicState, atomicCC) =>
            CausalStore((DotStore[A].empty, atomicState), atomicCC)
        }

        decomposed1 ++ decomposed2
    }

    override def empty: (A, B) = (DotStore[A].empty, DotStore[B].empty)
  }

  /** DotLess is a dot store implementation that, in combination with [[DotPair]], allows to compose non-causal CRDTs
    * with causal CRDTs. For a usage example, see [[interfaces.RGAInterface]], where the implicit presence of DotLess is
    * necessary so that the non-causal [[interfaces.ForcedWriteInterface]] can be part of the [[DotPair]] that makes up
    * the state.
    */
  type DotLess[A] = A
  implicit def DotLess[A: UIJDLattice]: DotStore[A] = new DotStore[A] {
    override def dots(ds: A): Set[Dot] = Set.empty[Dot]

    override def mergePartial(left: CausalStore[A], right: CausalStore[A]): A =
      Lattice[A].merge(left.store, right.store)

    override def leq(left: CausalStore[A], right: CausalStore[A]): Boolean =
      UIJDLattice[A].leq(left.store, right.store)

    override def decompose(state: CausalStore[A]): Iterable[CausalStore[A]] = {
      UIJDLattice[A].decompose(state.store).map(CausalStore(_, CausalContext.empty))
    }

    override def empty: A = UIJDLattice[A].bottom
  }
}
