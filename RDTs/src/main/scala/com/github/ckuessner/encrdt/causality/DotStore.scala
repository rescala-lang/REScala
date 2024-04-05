package com.github.ckuessner.encrdt.causality

import com.github.ckuessner.encrdt.causality.impl.ArrayCausalContext
import DotStore._

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
sealed trait DotStore[D] {
  def dots(dotStore: D): DotSet

  def bottom: D
}

object DotStore {
  type Dot = LamportClock
  type DotSet = ArrayCausalContext
  type DotFun[V] = Map[Dot, V]
  type DotMap[K, V] = Map[K, V]

  def apply[D](implicit dotStore: DotStore[D]): DotStore[D] = dotStore

  implicit def dotSetDotStore: DotStore[DotSet] = new DotStore[DotSet] {
    override def dots(dotStore: DotSet): DotSet = dotStore

    override def bottom: DotSet = ArrayCausalContext.empty
  }

  // Todo: V should be a SemiLattice according to paper
  implicit def dotFunDotStore[V]: DotStore[DotFun[V]] = new DotStore[DotFun[V]] {
    override def dots(dotFun: DotFun[V]): DotSet = ArrayCausalContext.fromSet(dotFun.keySet)

    override def bottom: DotFun[V] = Map.empty
  }

  implicit def dotMapDotStore[K, V: DotStore]: DotStore[DotMap[K, V]] = new DotStore[DotMap[K, V]] {
    override def dots(dotStore: DotMap[K, V]): DotSet =
      dotStore.values.map(DotStore[V].dots(_)).reduce((l,r) => l union r)

    override def bottom: DotMap[K, V] = Map.empty
  }
}

// TODO: Remove?
object DotSetPartialOrdering extends PartialOrdering[DotSet] {
  override def tryCompare(x: DotSet, y: DotSet): Option[Int] = {
    val unionOfXandY = x union y
    if (unionOfXandY == x) { // x contains all elements of union of x and y
      if (unionOfXandY == y) return Some(0) // x = y
      else return Some(1) // y doesn't contain all elements of x => x > y
    } else if (unionOfXandY == y) return Some(1) // y contains all elements of union of x and y but x doesn't
    else return None
  }

  override def lteq(x: DotSet, y: DotSet): Boolean = {
    x.forall(y.contains)
  }
}
