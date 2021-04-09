package rescala.extra.lattices.delta

import rescala.extra.lattices.Lattice

trait DotStore[A] {
  def dots(ds: A): Set[Dot]

  def merge[C: CContext, D: CContext](left: A, leftContext: C, right: A, rightContext: D): (A, C)

  def leq[C: CContext](left: A, leftContext: C, right: A, rightContext: C): Boolean

  def decompose[C: CContext](state: A, cc: C): Set[(A, C)]

  def bottom: A
}

object DotStore {
  def apply[A](implicit ds: DotStore[A]): DotStore[A] = ds

  implicit def DotStoreAsUIJDLattice[A: DotStore, C: CContext]: UIJDLattice[(A, C)] = new UIJDLattice[(A, C)] {
    override def leq(left: (A, C), right: (A, C)): Boolean = DotStore[A].leq[C](left._1, left._2, right._1, right._2)

    /**
     * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
     */
    override def decompose(state: (A, C)): Set[(A, C)] = DotStore[A].decompose[C](state._1, state._2)

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: (A, C), right: (A, C)): (A, C) = DotStore[A].merge(left._1, left._2, right._1, right._2)
  }

  type DotSet = Set[Dot]
  implicit def DotSet: DotStore[Set[Dot]] = new DotStore[Set[Dot]] {
    override def dots(ds: Set[Dot]): Set[Dot] = ds

    override def merge[C: CContext, D: CContext](left: Set[Dot], leftContext: C, right: Set[Dot], rightContext: D): (Set[Dot], C) = {
      val inBoth = left intersect right
      val fromLeft = left.filter(!CContext[D].contains(rightContext, _))
      val fromRight = right.filter(!CContext[C].contains(leftContext, _))

      (inBoth union fromLeft union fromRight, CContext[C].union(leftContext, rightContext))
    }

    override def bottom: Set[Dot] = Set.empty[Dot]

    override def leq[C: CContext](left: Set[Dot], leftContext: C, right: Set[Dot], rightContext: C): Boolean = {
      val firstCondition = CContext[C].toSet(leftContext).forall(CContext[C].contains(rightContext, _))
      val secondCondition = (right intersect (CContext[C].toSet(leftContext) diff left)).isEmpty
      firstCondition && secondCondition
    }

    override def decompose[C: CContext](state: Set[Dot], cc: C): Set[(Set[Dot], C)] = {
      val added = for (d <- state) yield (Set(d), CContext[C].fromSet(Set(d)))
      val removed = for (d <- CContext[C].toSet(cc) diff state) yield (DotSet.bottom, CContext[C].fromSet(Set(d)))
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

    override def bottom: Map[Dot, A] = Map.empty[Dot, A]

    override def leq[C: CContext](left: Map[Dot, A], leftContext: C, right: Map[Dot, A], rightContext: C): Boolean = {
      val firstCondition = CContext[C].toSet(leftContext).forall(CContext[C].contains(rightContext, _))
      val secondCondition = (left.keySet intersect right.keySet).forall(k => UIJDLattice[A].leq(left(k), right(k)))
      val thirdCondition = (DotFun[A].dots(right) intersect (CContext[C].toSet(leftContext) diff DotFun[A].dots(left))).isEmpty
      firstCondition && secondCondition && thirdCondition
    }

    override def decompose[C: CContext](state: Map[Dot, A], cc: C): Set[(Map[Dot, A], C)] = {
      val added = for (d <- DotFun[A].dots(state); v <- UIJDLattice[A].decompose(state(d))) yield (Map(d -> v), CContext[C].fromSet(Set(d)))
      val removed = for (d <- CContext[C].toSet(cc) diff DotFun[A].dots(state)) yield (DotFun[A].bottom, CContext[C].fromSet(Set(d)))
      added union removed
    }
  }

  type DotMap[K, V] = Map.WithDefault[K, V]
  implicit def DotMap[K, V: DotStore]: DotStore[DotMap[K, V]] = new DotStore[DotMap[K, V]] {
    override def dots(ds: DotMap[K, V]): Set[Dot] = ds.values.flatMap(DotStore[V].dots(_)).toSet

    override def merge[C: CContext, D: CContext](left: DotMap[K, V], leftContext: C, right: DotMap[K, V], rightContext: D): (DotMap[K, V], C) = {
      val allKeys = left.keySet union right.keySet
      val mergedValues = allKeys map { k =>
        val (mergedVal, _) = DotStore[V].merge(left(k), leftContext, right(k), rightContext)
        k -> mergedVal
      } filter { case (_, v) => v != DotStore[V].bottom }

      (new DotMap(mergedValues.toMap, _ => DotStore[V].bottom), CContext[C].union(leftContext, rightContext))
    }

    override def bottom: DotMap[K, V] = new DotMap(Map.empty[K, V], _ => DotStore[V].bottom)

    override def leq[C: CContext](left: DotMap[K, V], leftContext: C, right: DotMap[K, V], rightContext: C): Boolean = {
      val firstCondition = CContext[C].toSet(leftContext).forall(CContext[C].contains(rightContext, _))
      val secondCondition = (left.keySet union right.keySet).forall(k => DotStoreAsUIJDLattice[V, C].leq((left(k), leftContext), (right(k), rightContext)))
      firstCondition && secondCondition
    }

    override def decompose[C: CContext](state: DotMap[K, V], cc: C): Set[(DotMap[K, V], C)] = {
      val added = for (
        k <- state.keySet;
        (atomicV, atomicCC) <- UIJDLattice[(V, C)].decompose((state(k), CContext[C].fromSet(DotStore[V].dots(state(k)))))
      ) yield (DotMap[K, V].bottom.updated(k, atomicV), atomicCC)
      val removed = for (d <- CContext[C].toSet(cc) diff DotMap[K, V].dots(state)) yield (DotMap[K, V].bottom, CContext[C].fromSet(Set(d)))
      added union removed
    }
  }
}
