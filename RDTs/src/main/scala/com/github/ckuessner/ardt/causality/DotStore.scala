package com.github.ckuessner.ardt.causality

import com.github.ckuessner.ardt.causality.impl.ArrayCausalContext
import DotStore.*

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
sealed trait DotStore[D] {
  def dots(dotStore: D): DotSet

  def bottom: D
}

object DotStore {
  type DotSet       = ArrayCausalContext
  type DotFun[V]    = Map[Dot, V]
  type DotMap[K, V] = Map[K, V]

  inline def apply[D](using dotStore: DotStore[D]): DotStore[D] = dotStore

  given dotSetDotStore: DotStore[DotSet] = new DotStore[DotSet] {
    override def dots(dotStore: DotSet): DotSet = dotStore

    override def bottom: DotSet = ArrayCausalContext.empty
  }

  // Todo: V should be a SemiLattice according to paper
  given dotFunDotStore[V]: DotStore[DotFun[V]] = new DotStore[DotFun[V]] {
    override def dots(dotFun: DotFun[V]): DotSet = ArrayCausalContext.fromSet(dotFun.keySet)

    override def bottom: DotFun[V] = Map.empty
  }

  given dotMapDotStore[K, V: DotStore]: DotStore[DotMap[K, V]] = new DotStore[DotMap[K, V]] {
    override def dots(dotStore: DotMap[K, V]): DotSet =
      dotStore.values.map(DotStore[V].dots(_)).reduce((l, r) => l.union(r))

    override def bottom: DotMap[K, V] = Map.empty
  }

  // TODO: Remove?
  given DotSetPartialOrdering: PartialOrdering[DotSet] with {
    override def tryCompare(x: DotSet, y: DotSet): Option[Int] = {
      val unionOfXandY = x.union(y)
      if (unionOfXandY == x) {              // x contains all elements of union of x and y
        if (unionOfXandY == y) Some(0)      // x = y
        else Some(1)                        // y doesn't contain all elements of x => x > y
      } else if (unionOfXandY == y) Some(1) // y contains all elements of union of x and y but x doesn't
      else None
    }

    override def lteq(x: DotSet, y: DotSet): Boolean = {
      x.forall(y.contains)
    }
  }
}
