package kofre.decompose

import kofre.Lattice
import kofre.causality.{CContext, Dot}

/** DotStore is the typeclass trait for dot stores, data structures that are part of causal CRDTs and make use of dots to
  * track causality.
  */
trait DotStore[A] {
  def dots(state: A): Set[Dot]

  def mergePartial[C: CContext](left: Causal[A, C], right: Causal[A, C]): A

  def leq[C: CContext](left: Causal[A, C], right: Causal[A, C]): Boolean

  def decompose[C: CContext](state: Causal[A, C]): Iterable[Causal[A, C]]

  def empty: A
}

object DotStore {
  def apply[A](implicit ds: DotStore[A]): DotStore[A] = ds

  /** DotSet is a dot store implementation that is simply a set of dots. See [[interfaces.EWFlagInterface]] for a
    * usage example.
    */
  type DotSet = Set[Dot]
  implicit def DotSet: DotStore[Set[Dot]] = new DotStore[Set[Dot]] {
    override def dots(ds: Set[Dot]): Set[Dot] = ds

    override def mergePartial[C: CContext](left: Causal[Set[Dot], C], right: Causal[Set[Dot], C]): Set[Dot] = {
      val fromLeft  = left.dotStore.filter(!CContext[C].contains(right.cc, _))
      val fromRight = right.dotStore.filter(dot => left.dotStore.contains(dot) || !CContext[C].contains(left.cc, dot))

      fromLeft union fromRight
    }

    override def empty: Set[Dot] = Set.empty[Dot]

    override def leq[C: CContext](left: Causal[Set[Dot], C], right: Causal[Set[Dot], C]): Boolean = {
      val firstCondition = CContext[C].forall(left.cc, CContext[C].contains(right.cc, _))

      val secondCondition = {
        val diff = CContext[C].diff(left.cc, left.dotStore)
        !right.dotStore.exists { CContext[C].contains(diff, _) }
      }

      firstCondition && secondCondition
    }

    override def decompose[C: CContext](state: Causal[Set[Dot], C]): Iterable[Causal[Set[Dot], C]] = {
      val added   = for (d <- state.dotStore) yield Causal(Set(d), CContext[C].one(d))
      val removed = CContext[C].decompose(state.cc, state.dotStore.contains).map(Causal(DotSet.empty, _))
      removed ++ added
    }
  }

  /** DotFun is a dot store implementation that maps dots to values of a Lattice type. See [[interfaces.MVRegisterInterface]]
    * for a usage example.
    */
  type DotFun[A] = Map[Dot, A]
  implicit def DotFun[A: UIJDLattice]: DotStore[Map[Dot, A]] = new DotStore[Map[Dot, A]] {
    override def dots(ds: Map[Dot, A]): Set[Dot] = ds.keySet

    override def mergePartial[C: CContext](left: Causal[Map[Dot, A], C], right: Causal[Map[Dot, A], C]): Map[Dot, A] = {
      val fromLeft = left.dotStore.filter { case (dot, _) => !CContext[C].contains(right.cc, dot) }

      right.dotStore.foldLeft(fromLeft) {
        case (m, (dot, r)) =>
          left.dotStore.get(dot) match {
            case None =>
              if (CContext[C].contains(left.cc, dot)) m
              else m.updated(dot, r)
            case Some(l) => m.updated(dot, UIJDLattice[A].merge(l, r))
          }
      }
    }

    override def empty: Map[Dot, A] = Map.empty[Dot, A]

    override def leq[C: CContext](left: Causal[Map[Dot, A], C], right: Causal[Map[Dot, A], C]): Boolean = {
      val firstCondition = CContext[C].forall(left.cc, CContext[C].contains(right.cc, _))
      val secondCondition = right.dotStore.keySet.forall { k =>
        left.dotStore.get(k).forall { l => UIJDLattice[A].leq(l, right.dotStore(k)) }
      }
      val thirdCondition = {
        val diff = CContext[C].diff(left.cc, DotFun[A].dots(left.dotStore))
        !DotFun[A].dots(right.dotStore).exists { CContext[C].contains(diff, _) }
      }

      firstCondition && secondCondition && thirdCondition
    }

    override def decompose[C: CContext](state: Causal[Map[Dot, A], C]): Iterable[Causal[Map[Dot, A], C]] = {
      val added = for (d <- DotFun[A].dots(state.dotStore); v <- UIJDLattice[A].decompose(state.dotStore(d)))
        yield Causal(Map(d -> v), CContext[C].one(d))

      val removed =
        CContext[C].decompose(state.cc, DotFun[A].dots(state.dotStore).contains).map(Causal(DotFun[A].empty, _))

      removed ++ added
    }
  }

  /** DotMap is a dot store implementation that maps keys of an arbitrary type K to values of a dot store type V. See
    * [[interfaces.ORMapInterface]] for a usage exmample.
    */
  type DotMap[K, V] = Map[K, V]
  implicit def DotMap[K, V: DotStore]: DotStore[DotMap[K, V]] = new DotStore[DotMap[K, V]] {
    override def dots(ds: DotMap[K, V]): Set[Dot] = ds.values.flatMap(DotStore[V].dots(_)).toSet

    override def mergePartial[C: CContext](
        left: Causal[DotMap[K, V], C],
        right: Causal[DotMap[K, V], C]
    ): DotMap[K, V] = {
      def mergeHelp(l: V, r: V): Option[V] = {
        val mergedVal = DotStore[V].mergePartial(Causal(l, left.cc), Causal(r, right.cc))
        if (mergedVal == DotStore[V].empty) None
        else Some(mergedVal)
      }

      var rightSet = CContext[C].toSet(right.cc)

      val added = right.dotStore.foldLeft(left.dotStore) { case (currentLeft, (k, r)) =>
        rightSet --= DotStore[V].dots(r)
        currentLeft.updatedWith(k) {
          case None    => mergeHelp(DotStore[V].empty, r)
          case Some(l) => mergeHelp(l, r)
        }
      }

      if (rightSet.isEmpty) added
      else {
        added.foldLeft(added) { case (current, (k, l)) =>
          if (right.dotStore.contains(k)) current
          else mergeHelp(l, DotStore[V].empty) match {
            case None         => current.removed(k)
            case Some(merged) => current.updated(k, merged)
          }
        }
      }
    }

    override def empty: DotMap[K, V] = Map.empty[K, V]

    override def leq[C: CContext](left: Causal[DotMap[K, V], C], right: Causal[DotMap[K, V], C]): Boolean = {
      val firstCondition = CContext[C].forall(left.cc, CContext[C].contains(right.cc, _))

      def secondConditionHelper(keys: Iterable[K]): Boolean = keys.forall { k =>
        val leftV  = left.dotStore.getOrElse(k, DotStore[V].empty)
        val rightV = right.dotStore.getOrElse(k, DotStore[V].empty)

        DotStore[V].leq[C](Causal(leftV, left.cc), Causal(rightV, right.cc))
      }

      val secondCondition = secondConditionHelper(left.dotStore.keys) && secondConditionHelper(right.dotStore.keys)

      firstCondition && secondCondition
    }

    override def decompose[C: CContext](state: Causal[DotMap[K, V], C]): Iterable[Causal[DotMap[K, V], C]] = {
      val added = for {
        k <- state.dotStore.keys
        Causal(atomicV, atomicCC) <- {
          val v = state.dotStore.getOrElse(k, DotStore[V].empty)
          DotStore[V].decompose(Causal(v, CContext[C].fromSet(DotStore[V].dots(v))))
        }
      } yield Causal(DotMap[K, V].empty.updated(k, atomicV), atomicCC)

      val removed =
        CContext[C].decompose(state.cc, DotMap[K, V].dots(state.dotStore).contains).map(Causal(DotMap[K, V].empty, _))

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

    override def mergePartial[C: CContext](left: Causal[(A, B), C], right: Causal[(A, B), C]): (A, B) =
      (left, right) match {
        case (Causal((left1, left2), leftCContext), Causal((right1, right2), rightCContext)) =>
          val stateMerged1 = DotStore[A].mergePartial(Causal(left1, leftCContext), Causal(right1, rightCContext))
          val stateMerged2 = DotStore[B].mergePartial(Causal(left2, leftCContext), Causal(right2, rightCContext))

          (stateMerged1, stateMerged2)
      }

    override def leq[C: CContext](left: Causal[(A, B), C], right: Causal[(A, B), C]): Boolean =
      (left, right) match {
        case (Causal((left1, left2), leftCContext), Causal((right1, right2), rightCContext)) =>
          DotStore[A].leq(Causal(left1, leftCContext), Causal(right1, rightCContext)) &&
          DotStore[B].leq(Causal(left2, leftCContext), Causal(right2, rightCContext))
      }

    override def decompose[C: CContext](state: Causal[(A, B), C]): Iterable[Causal[(A, B), C]] = state match {
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

    override def mergePartial[C: CContext](left: Causal[A, C], right: Causal[A, C]): A =
      Lattice[A].merge(left.dotStore, right.dotStore)

    override def leq[C: CContext](left: Causal[A, C], right: Causal[A, C]): Boolean =
      UIJDLattice[A].leq(left.dotStore, right.dotStore)

    override def decompose[C: CContext](state: Causal[A, C]): Iterable[Causal[A, C]] = {
      UIJDLattice[A].decompose(state.dotStore).map(Causal(_, CContext[C].empty))
    }

    override def empty: A = UIJDLattice[A].bottom
  }
}
