package rescala.extra.lattices.delta

import rescala.extra.lattices.Lattice

trait DotStore[A] {
  def dots(ds: A): Set[Dot]

  def merge[C: CContext, D: CContext](left: A, leftContext: C, right: A, rightContext: D): (A, C)

  def bottom: A
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

    override def bottom: Set[Dot] = Set.empty[Dot]
  }

  type DotFun[A] = Map[Dot, A]
  implicit def DotFun[A: Lattice]: DotStore[Map[Dot, A]] = new DotStore[Map[Dot, A]] {
    override def dots(ds: Map[Dot, A]): Set[Dot] = ds.keySet

    override def merge[C: CContext, D: CContext](left: Map[Dot, A], leftContext: C, right: Map[Dot, A], rightContext: D): (Map[Dot, A], C) = {
      val inBoth = (left.keySet intersect right.keySet).map(dot => dot -> Lattice.merge(left(dot), right(dot))).toMap
      val fromLeft = left.filter { case (dot, _) => !CContext[D].contains(rightContext, dot) }
      val fromRight = right.filter { case (dot, _) => !CContext[C].contains(leftContext, dot) }

      (inBoth ++ fromLeft ++ fromRight, CContext[C].union(leftContext, rightContext))
    }

    override def bottom: Map[Dot, A] = Map.empty[Dot, A]
  }

  type DotMap[K, V] = Map.WithDefault[K, V]
  implicit def DotMap[K, V: DotStore]: DotStore[Map.WithDefault[K, V]] = new DotStore[Map.WithDefault[K, V]] {
    override def dots(ds: Map.WithDefault[K, V]): Set[Dot] = ds.values.flatMap(DotStore[V].dots(_)).toSet

    override def merge[C: CContext, D: CContext](left: Map.WithDefault[K, V], leftContext: C, right: Map.WithDefault[K, V], rightContext: D): (Map.WithDefault[K, V], C) = {
      val allKeys = left.keySet union right.keySet
      val mergedValues = allKeys map { k =>
        val (mergedVal, _) = DotStore[V].merge(left(k), leftContext, right(k), rightContext)
        k -> mergedVal
      } filter { case (_, v) => v != DotStore[V].bottom }

      (new Map.WithDefault(mergedValues.toMap, _ => DotStore[V].bottom), CContext[C].union(leftContext, rightContext))
    }

    override def bottom: Map.WithDefault[K, V] = new Map.WithDefault(Map.empty[K, V], _ => DotStore[V].bottom)
  }
}
