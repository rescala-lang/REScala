package kofre.decompose

import kofre.Lattice
import kofre.causality.{CContext, Causal, CausalContext, Dot}

/** DotStore is the typeclass trait for dot stores, data structures that are part of causal CRDTs and make use of dots to
  * track causality.
  */
trait DotStore[A] {
  def dots(state: A): Set[Dot]

  def mergePartial(left: Causal[A], right: Causal[A]): A

  def leq(left: Causal[A], right: Causal[A]): Boolean

  def decompose(state: Causal[A]): Iterable[Causal[A]]

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

    override def mergePartial(left: Causal[Set[Dot]], right: Causal[Set[Dot]]): Set[Dot] = {
      val fromLeft  = left.store.filter(!CContext[C].contains(right.context, _))
      val fromRight = right.store.filter(dot => left.store.contains(dot) || !CContext[C].contains(left.context, dot))

      fromLeft union fromRight
    }

    override def empty: Set[Dot] = Set.empty[Dot]

    override def leq(left: Causal[Set[Dot]], right: Causal[Set[Dot]]): Boolean = {
      val firstCondition = CContext[C].forall(left.context, CContext[C].contains(right.context, _))

      val secondCondition = {
        val diff = CContext[C].diff(left.context, left.store)
        !right.store.exists {CContext[C].contains(diff, _) }
      }

      firstCondition && secondCondition
    }

    override def decompose(state: Causal[Set[Dot]]): Iterable[Causal[Set[Dot]]] = {
      val added   = for (d <- state.store) yield Causal(Set(d), CContext[C].one(d))
      val removed = CContext[C].decompose(state.context, state.store.contains).map(Causal(DotSet.empty, _))
      removed ++ added
    }
  }

  /** DotFun is a dot store implementation that maps dots to values of a Lattice type. See [[interfaces.MVRegisterInterface]]
    * for a usage example.
    */
  type DotFun[A] = Map[Dot, A]
  implicit def DotFun[A: UIJDLattice]: DotStore[Map[Dot, A]] = new DotStore[Map[Dot, A]] {
    override def dots(ds: Map[Dot, A]): Set[Dot] = ds.keySet

    override def mergePartial(left: Causal[Map[Dot, A]], right: Causal[Map[Dot, A]]): Map[Dot, A] = {
      val fromLeft = left.store.filter { case (dot, _) => !CContext[C].contains(right.context, dot) }

      right.store.foldLeft(fromLeft) {
        case (m, (dot, r)) =>
          left.store.get(dot) match {
            case None =>
              if (CContext[C].contains(left.context, dot)) m
              else m.updated(dot, r)
            case Some(l) => m.updated(dot, UIJDLattice[A].merge(l, r))
          }
      }
    }

    override def empty: Map[Dot, A] = Map.empty[Dot, A]

    override def leq(left: Causal[Map[Dot, A]], right: Causal[Map[Dot, A]]): Boolean = {
      val firstCondition = CContext[C].forall(left.context, CContext[C].contains(right.context, _))
      val secondCondition = right.store.keySet.forall { k =>
        left.store.get(k).forall { l => UIJDLattice[A].leq(l, right.store(k)) }
      }
      val thirdCondition = {
        val diff = CContext[C].diff(left.context, DotFun[A].dots(left.store))
        !DotFun[A].dots(right.store).exists {CContext[C].contains(diff, _) }
      }

      firstCondition && secondCondition && thirdCondition
    }

    override def decompose(state: Causal[Map[Dot, A]]): Iterable[Causal[Map[Dot, A]]] = {
      val added = for (d <- DotFun[A].dots(state.store); v <- UIJDLattice[A].decompose(state.store(d)))
        yield Causal(Map(d -> v), CContext[C].one(d))

      val removed =
        CContext[C].decompose(state.context, DotFun[A].dots(state.store).contains).map(Causal(DotFun[A].empty, _))

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
        left: Causal[DotMap[K, V]],
        right: Causal[DotMap[K, V]]
    ): DotMap[K, V] = {
      def mergeHelp(l: V, r: V): Option[V] = {
        val mergedVal = DotStore[V].mergePartial(Causal(l, left.context), Causal(r, right.context))
        if (mergedVal == DotStore[V].empty) None
        else Some(mergedVal)
      }

      var rightSet = CContext[C].toSet(right.context)

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

    override def leq(left: Causal[DotMap[K, V]], right: Causal[DotMap[K, V]]): Boolean = {
      val firstCondition = CContext[C].forall(left.context, CContext[C].contains(right.context, _))

      def secondConditionHelper(keys: Iterable[K]): Boolean = keys.forall { k =>
        val leftV  = left.store.getOrElse(k, DotStore[V].empty)
        val rightV = right.store.getOrElse(k, DotStore[V].empty)

        DotStore[V].leq(Causal(leftV, left.context), Causal(rightV, right.context))
      }

      val secondCondition = secondConditionHelper(left.store.keys) && secondConditionHelper(right.store.keys)

      firstCondition && secondCondition
    }

    override def decompose(state: Causal[DotMap[K, V]]): Iterable[Causal[DotMap[K, V]]] = {
      val added = for {
        k <- state.store.keys
        Causal(atomicV, atomicCC) <- {
          val v = state.store.getOrElse(k, DotStore[V].empty)
          DotStore[V].decompose(Causal(v, CContext[C].fromSet(DotStore[V].dots(v))))
        }
      } yield Causal(DotMap[K, V].empty.updated(k, atomicV), atomicCC)

      val removed =
        CContext[C].decompose(state.context, DotMap[K, V].dots(state.store).contains).map(Causal(DotMap[K, V].empty, _))

      added ++ removed
    }
  }

  /** DotPair is a dot store implementation that allows the composition of two dot stores in a pair. See [[interfaces.RGAInterface]]
    * for a usage example
    */
  type DotPair[A, B] = (A, B)
  implicit def DotPair[A: DotStore, B: DotStore]: DotStore[DotPair[A, B]] = new DotStore[(A, B)] {
    override def dots(ds: (A, B)): Set[Dot] = ds match {
      case (ds1, ds2) => DotStore[A].dots(ds1) union DotStore[B].dots(ds2)
    }

    override def mergePartial(left: Causal[(A, B)], right: Causal[(A, B)]): (A, B) =
      (left, right) match {
        case (Causal((left1, left2), leftCContext), Causal((right1, right2), rightCContext)) =>
          val stateMerged1 = DotStore[A].mergePartial(Causal(left1, leftCContext), Causal(right1, rightCContext))
          val stateMerged2 = DotStore[B].mergePartial(Causal(left2, leftCContext), Causal(right2, rightCContext))

          (stateMerged1, stateMerged2)
      }

    override def leq(left: Causal[(A, B)], right: Causal[(A, B)]): Boolean =
      (left, right) match {
        case (Causal((left1, left2), leftCContext), Causal((right1, right2), rightCContext)) =>
          DotStore[A].leq(Causal(left1, leftCContext), Causal(right1, rightCContext)) &&
          DotStore[B].leq(Causal(left2, leftCContext), Causal(right2, rightCContext))
      }

    override def decompose(state: Causal[(A, B)]): Iterable[Causal[(A, B)]] = state match {
      case Causal((state1, state2), cc) =>
        val decomposed1 = DotStore[A].decompose(Causal(state1, cc)).map {
          case Causal(atomicState, atomicCC) =>
            Causal((atomicState, DotStore[B].empty), atomicCC)
        }

        val decomposed2 = DotStore[B].decompose(Causal(state2, cc)).map {
          case Causal(atomicState, atomicCC) =>
            Causal((DotStore[A].empty, atomicState), atomicCC)
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

    override def mergePartial(left: Causal[A], right: Causal[A]): A =
      Lattice[A].merge(left.store, right.store)

    override def leq(left: Causal[A], right: Causal[A]): Boolean =
      UIJDLattice[A].leq(left.store, right.store)

    override def decompose(state: Causal[A]): Iterable[Causal[A]] = {
      UIJDLattice[A].decompose(state.store).map(Causal(_, CContext[C].empty))
    }

    override def empty: A = UIJDLattice[A].bottom
  }
}
