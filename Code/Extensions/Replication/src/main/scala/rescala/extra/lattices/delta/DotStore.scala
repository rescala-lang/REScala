package rescala.extra.lattices.delta

import rescala.extra.lattices.Lattice

/** DotStore is the typeclass trait for dot stores, data structures that are part of causal CRDTs and make use of dots to
  * track causality.
  */
trait DotStore[A] {
  def dots(state: A): Set[Dot]

  def merge[C: CContext](left: A, leftContext: C, right: A, rightContext: C): (A, C)

  def leq[C: CContext](left: A, leftContext: C, right: A, rightContext: C): Boolean

  def decompose[C: CContext](state: A, cc: C): Iterable[(A, C)]

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

    override def merge[C: CContext](left: Set[Dot], leftContext: C, right: Set[Dot], rightContext: C): (Set[Dot], C) = {
      val fromLeft  = left.filter(!CContext[C].contains(rightContext, _))
      val fromRight = right.filter(dot => left.contains(dot) || !CContext[C].contains(leftContext, dot))

      (fromLeft union fromRight, CContext[C].union(leftContext, rightContext))
    }

    override def empty: Set[Dot] = Set.empty[Dot]

    override def leq[C: CContext](left: Set[Dot], leftContext: C, right: Set[Dot], rightContext: C): Boolean = {
      val firstCondition = CContext[C].forall(leftContext, CContext[C].contains(rightContext, _))

      val secondCondition = {
        val diff = CContext[C].diff(leftContext, left)
        !right.exists { CContext[C].contains(diff, _) }
      }

      firstCondition && secondCondition
    }

    override def decompose[C: CContext](state: Set[Dot], cc: C): Iterable[(Set[Dot], C)] = {
      val added   = for (d <- state) yield (Set(d), CContext[C].one(d))
      val removed = CContext[C].decompose(cc, state.contains).map((DotSet.empty, _))
      removed ++ added
    }
  }

  /** DotFun is a dot store implementation that maps dots to values of a Lattice type. See [[interfaces.MVRegisterInterface]]
    * for a usage example.
    */
  type DotFun[A] = Map[Dot, A]
  implicit def DotFun[A: UIJDLattice]: DotStore[Map[Dot, A]] = new DotStore[Map[Dot, A]] {
    override def dots(ds: Map[Dot, A]): Set[Dot] = ds.keySet

    override def merge[C: CContext](left: Map[Dot, A], leftContext: C, right: Map[Dot, A], rightContext: C)
        : (Map[Dot, A], C) = {

      val fromLeft = left.filter { case (dot, _) => !CContext[C].contains(rightContext, dot) }

      val dfMerged = right.foldLeft(fromLeft) {
        case (m, (dot, r)) =>
          left.get(dot) match {
            case None =>
              if (CContext[C].contains(leftContext, dot)) m
              else m.updated(dot, r)
            case Some(l) => m.updated(dot, UIJDLattice[A].merge(l, r))
          }
      }

      (dfMerged, CContext[C].union(leftContext, rightContext))
    }

    override def empty: Map[Dot, A] = Map.empty[Dot, A]

    override def leq[C: CContext](left: Map[Dot, A], leftContext: C, right: Map[Dot, A], rightContext: C): Boolean = {
      val firstCondition = CContext[C].forall(leftContext, CContext[C].contains(rightContext, _))
      val secondCondition = right.keySet.forall { k =>
        left.get(k).forall { l => UIJDLattice[A].leq(l, right(k)) }
      }
      val thirdCondition = {
        val diff = CContext[C].diff(leftContext, DotFun[A].dots(left))
        !DotFun[A].dots(right).exists { CContext[C].contains(diff, _) }
      }

      firstCondition && secondCondition && thirdCondition
    }

    override def decompose[C: CContext](state: Map[Dot, A], cc: C): Iterable[(Map[Dot, A], C)] = {
      val added = for (d <- DotFun[A].dots(state); v <- UIJDLattice[A].decompose(state(d)))
        yield (Map(d -> v), CContext[C].one(d))

      val removed = CContext[C].decompose(cc, DotFun[A].dots(state).contains).map((DotFun[A].empty, _))

      removed ++ added
    }
  }

  /** DotMap is a dot store implementation that maps keys of an arbitrary type K to values of a dot store type V. See
    * [[interfaces.ORMapInterface]] for a usage exmample.
    */
  type DotMap[K, V] = Map[K, V]
  implicit def DotMap[K, V: DotStore]: DotStore[DotMap[K, V]] = new DotStore[DotMap[K, V]] {
    override def dots(ds: DotMap[K, V]): Set[Dot] = ds.values.flatMap(DotStore[V].dots(_)).toSet

    override def merge[C: CContext](
        left: DotMap[K, V],
        leftContext: C,
        right: DotMap[K, V],
        rightContext: C
    ): (DotMap[K, V], C) = {
      val allKeys = left.keySet ++ right.keySet

      val dmMerged = allKeys.foldLeft(left) {
        case (m, k) =>
          val leftV          = left.getOrElse(k, DotStore[V].empty)
          val rightV         = right.getOrElse(k, DotStore[V].empty)
          val (mergedVal, _) = DotStore[V].merge(leftV, leftContext, rightV, rightContext)

          m.updated(k, mergedVal)
      }.filterNot {
        case (_, v) => v == DotStore[V].empty
      }

      (dmMerged, CContext[C].union(leftContext, rightContext))
    }

    override def empty: DotMap[K, V] = Map.empty[K, V]

    override def leq[C: CContext](left: DotMap[K, V], leftContext: C, right: DotMap[K, V], rightContext: C): Boolean = {
      val firstCondition = CContext[C].forall(leftContext, CContext[C].contains(rightContext, _))

      def secondConditionHelper(keys: Iterable[K]): Boolean = keys.forall { k =>
        val leftV  = left.getOrElse(k, DotStore[V].empty)
        val rightV = right.getOrElse(k, DotStore[V].empty)

        DotStore[V].leq[C](leftV, leftContext, rightV, rightContext)
      }

      val secondCondition = secondConditionHelper(left.keys) && secondConditionHelper(right.keys)

      firstCondition && secondCondition
    }

    override def decompose[C: CContext](state: DotMap[K, V], cc: C): Iterable[(DotMap[K, V], C)] = {
      val added = for {
        k <- state.keys
        (atomicV, atomicCC) <- {
          val v = state.getOrElse(k, DotStore[V].empty)
          DotStore[V].decompose(v, CContext[C].fromSet(DotStore[V].dots(v)))
        }
      } yield (DotMap[K, V].empty.updated(k, atomicV), atomicCC)

      val removed = CContext[C].decompose(cc, DotMap[K, V].dots(state).contains).map((DotMap[K, V].empty, _))

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

    override def merge[C: CContext](left: (A, B), leftContext: C, right: (A, B), rightContext: C): ((A, B), C) =
      (left, right) match {
        case ((left1, left2), (right1, right2)) =>
          val (stateMerged1, ccMerged1) = DotStore[A].merge(left1, leftContext, right1, rightContext)
          val (stateMerged2, ccMerged2) = DotStore[B].merge(left2, leftContext, right2, rightContext)

          ((stateMerged1, stateMerged2), CContext[C].union(ccMerged1, ccMerged2))
      }

    override def leq[C: CContext](left: (A, B), leftContext: C, right: (A, B), rightContext: C): Boolean =
      (left, right) match {
        case ((left1, left2), (right1, right2)) =>
          DotStore[A].leq(left1, leftContext, right1, rightContext) &&
            DotStore[B].leq(left2, leftContext, right2, rightContext)
      }

    override def decompose[C: CContext](state: (A, B), cc: C): Iterable[((A, B), C)] = state match {
      case (state1, state2) =>
        val decomposed1 = DotStore[A].decompose(state1, cc).map {
          case (atomicState, atomicCC) =>
            ((atomicState, DotStore[B].empty), atomicCC)
        }

        val decomposed2 = DotStore[B].decompose(state2, cc).map {
          case (atomicState, atomicCC) =>
            ((DotStore[A].empty, atomicState), atomicCC)
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

    override def merge[C: CContext](left: A, leftContext: C, right: A, rightContext: C): (A, C) =
      (Lattice[A].merge(left, right), CContext[C].union(leftContext, rightContext))

    override def leq[C: CContext](left: A, leftContext: C, right: A, rightContext: C): Boolean =
      UIJDLattice[A].leq(left, right)

    override def decompose[C: CContext](state: A, cc: C): Iterable[(A, C)] = {
      UIJDLattice[A].decompose(state).map((_, CContext[C].empty))
    }

    override def empty: A = UIJDLattice[A].bottom
  }
}
