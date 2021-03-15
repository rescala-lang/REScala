package rescala.extra.lattices.delta

import rescala.extra.lattices.Lattice

trait DotStore[A] {
  def dots(state: A): Set[Dot]

  def merge[C: CContext, D: CContext](left: A, leftContext: C, right: A, rightContext: D): (A, C)

  def leq[C: CContext, D: CContext](left: A, leftContext: C, right: A, rightContext: D): Boolean

  def decompose[C: CContext](state: A, cc: C): Set[(A, C)]

  def empty: A
}

object DotStore {
  def apply[A](implicit ds: DotStore[A]): DotStore[A] = ds

  type DotSet = Set[Dot]
  implicit def DotSet: DotStore[Set[Dot]] = new DotStore[Set[Dot]] {
    override def dots(ds: Set[Dot]): Set[Dot] = ds

    override def merge[C: CContext, D: CContext](left: Set[Dot], leftContext: C, right: Set[Dot], rightContext: D): (Set[Dot], C) = {
      val inBoth = left intersect right
      val fromLeft = left.filter(!CContext[D].contains(rightContext, _))
      val fromRight = right.filter(!CContext[C].contains(leftContext, _))

      (inBoth union fromLeft union fromRight, CContext[C].union(leftContext, rightContext))
    }

    override def empty: Set[Dot] = Set.empty[Dot]

    override def leq[C: CContext, D: CContext](left: Set[Dot], leftContext: C, right: Set[Dot], rightContext: D): Boolean = {
      val firstCondition = CContext[C].toSet(leftContext).forall(CContext[D].contains(rightContext, _))
      val secondCondition = (right intersect (CContext[C].toSet(leftContext) diff left)).isEmpty
      firstCondition && secondCondition
    }

    override def decompose[C: CContext](state: Set[Dot], cc: C): Set[(Set[Dot], C)] = {
      val added = for (d <- state) yield (Set(d), CContext[C].fromSet(Set(d)))
      val removed = for (d <- CContext[C].toSet(cc) diff state) yield (DotSet.empty, CContext[C].fromSet(Set(d)))
      added union removed
    }
  }

  type DotFun[A] = Map[Dot, A]
  implicit def DotFun[A: UIJDLattice]: DotStore[Map[Dot, A]] = new DotStore[Map[Dot, A]] {
    override def dots(ds: Map[Dot, A]): Set[Dot] = ds.keySet

    override def merge[C: CContext, D: CContext](left: Map[Dot, A], leftContext: C, right: Map[Dot, A], rightContext: D): (Map[Dot, A], C) = {
      val inBoth = (left.keySet intersect right.keySet).map(dot => dot -> Lattice.merge(left(dot), right(dot))).toMap
      val fromLeft = left.filter { case (dot, _) => !CContext[D].contains(rightContext, dot) }
      val fromRight = right.filter { case (dot, _) => !CContext[C].contains(leftContext, dot) }

      (inBoth ++ fromLeft ++ fromRight, CContext[C].union(leftContext, rightContext))
    }

    override def empty: Map[Dot, A] = Map.empty[Dot, A]

    override def leq[C: CContext, D: CContext](left: Map[Dot, A], leftContext: C, right: Map[Dot, A], rightContext: D): Boolean = {
      val firstCondition = CContext[C].toSet(leftContext).forall(CContext[D].contains(rightContext, _))
      val secondCondition = (left.keySet intersect right.keySet).forall(k => UIJDLattice[A].leq(left(k), right(k)))
      val thirdCondition = (DotFun[A].dots(right) intersect (CContext[C].toSet(leftContext) diff DotFun[A].dots(left))).isEmpty
      firstCondition && secondCondition && thirdCondition
    }

    override def decompose[C: CContext](state: Map[Dot, A], cc: C): Set[(Map[Dot, A], C)] = {
      val added = for (d <- DotFun[A].dots(state); v <- UIJDLattice[A].decompose(state(d))) yield (Map(d -> v), CContext[C].fromSet(Set(d)))
      val removed = for (d <- CContext[C].toSet(cc) diff DotFun[A].dots(state)) yield (DotFun[A].empty, CContext[C].fromSet(Set(d)))
      added union removed
    }
  }

  type DotMap[K, V] = Map[K, V]
  implicit def DotMap[K, V: DotStore]: DotStore[DotMap[K, V]] = new DotStore[DotMap[K, V]] {
    override def dots(ds: DotMap[K, V]): Set[Dot] = ds.values.flatMap(DotStore[V].dots(_)).toSet

    override def merge[C: CContext, D: CContext](left: DotMap[K, V], leftContext: C, right: DotMap[K, V], rightContext: D): (DotMap[K, V], C) = {
      val allKeys = left.keySet union right.keySet
      val mergedValues = allKeys map { k =>
        val leftV = left.getOrElse(k, DotStore[V].empty)
        val rightV = right.getOrElse(k, DotStore[V].empty)
        val (mergedVal, _) = DotStore[V].merge(leftV, leftContext, rightV, rightContext)

        k -> mergedVal
      } filter { case (_, v) => v != DotStore[V].empty }

      (mergedValues.toMap, CContext[C].union(leftContext, rightContext))
    }

    override def empty: DotMap[K, V] = Map.empty[K, V]

    override def leq[C: CContext, D: CContext](left: DotMap[K, V], leftContext: C, right: DotMap[K, V], rightContext: D): Boolean = {
      val firstCondition = CContext[C].toSet(leftContext).forall(CContext[D].contains(rightContext, _))
      val secondCondition = (left.keySet union right.keySet).forall { k =>
        val leftV = left.getOrElse(k, DotStore[V].empty)
        val rightV = right.getOrElse(k, DotStore[V].empty)

        DotStore[V].leq[C, D](leftV, leftContext, rightV, rightContext)
      }
      firstCondition && secondCondition
    }

    override def decompose[C: CContext](state: DotMap[K, V], cc: C): Set[(DotMap[K, V], C)] = {
      val added = for {
        k <- state.keySet
        (atomicV, atomicCC) <- {
          val v = state.getOrElse(k, DotStore[V].empty)
          DotStore[V].decompose(v, CContext[C].fromSet(DotStore[V].dots(v)))
        }
      } yield (DotMap[K, V].empty.updated(k, atomicV), atomicCC)
      val removed = for (d <- CContext[C].toSet(cc) diff DotMap[K, V].dots(state)) yield (DotMap[K, V].empty, CContext[C].fromSet(Set(d)))
      added union removed
    }
  }

  type DotPair[A, B] = (A, B)
  implicit def DotPair[A: DotStore, B: DotStore]: DotStore[DotPair[A, B]] = new DotStore[(A, B)] {
    override def dots(ds: (A, B)): Set[Dot] = ds match {
      case (ds1, ds2) => DotStore[A].dots(ds1) union DotStore[B].dots(ds2)
    }

    override def merge[C: CContext, D: CContext](left: (A, B), leftContext: C, right: (A, B), rightContext: D): ((A, B), C) = (left, right) match {
      case ((left1, left2), (right1, right2)) =>
        val (stateMerged1, ccMerged1) = DotStore[A].merge(left1, leftContext, right1, rightContext)
        val (stateMerged2, ccMerged2) = DotStore[B].merge(left2, leftContext, right2, rightContext)
        ((stateMerged1, stateMerged2), CContext[C].union(ccMerged1, ccMerged2))
    }

    override def leq[C: CContext, D: CContext](left: (A, B), leftContext: C, right: (A, B), rightContext: D): Boolean = (left, right) match {
      case ((left1, left2), (right1, right2)) =>
        DotStore[A].leq(left1, leftContext, right1, rightContext) &&
          DotStore[B].leq(left2, leftContext, right2, rightContext)
    }

    override def decompose[C: CContext](state: (A, B), cc: C): Set[((A, B), C)] = state match {
      case (state1, state2) =>
        val decomposed1 = DotStore[A].decompose(state1, cc).map{
          case (atomicState, atomicCC) =>
          ((atomicState, DotStore[B].empty), atomicCC)
        }

        val decomposed2 = DotStore[B].decompose(state2, cc).map{
          case (atomicState, atomicCC) =>
            ((DotStore[A].empty, atomicState), atomicCC)
        }

        decomposed1 union decomposed2
    }

    override def empty: (A, B) = (DotStore[A].empty, DotStore[B].empty)
  }

  type DotLess[A] = A
  implicit def DotLess[A: UIJDLattice]: DotStore[A] = new DotStore[A] {
    override def dots(ds: A): Set[Dot] = Set.empty[Dot]

    override def merge[C: CContext, D: CContext](left: A, leftContext: C, right: A, rightContext: D): (A, C) =
      (Lattice[A].merge(left, right), CContext[C].union[D](leftContext, rightContext))

    override def leq[C: CContext, D: CContext](left: A, leftContext: C, right: A, rightContext: D): Boolean =
      UIJDLattice[A].leq(left, right)

    override def decompose[C: CContext](state: A, cc: C): Set[(A, C)] = {
      UIJDLattice[A].decompose(state).map( (_, CContext[C].empty) )
    }

    override def empty: A = UIJDLattice[A].bottom
  }
}
