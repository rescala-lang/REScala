package de.ckuessner
package encrdt.causality

import encrdt.causality.DotStore._
import encrdt.lattices.SemiLattice

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
trait DotStore[D] {
  def dots(dotStore: D): Set[Dot]

  def bottom: D
}

object DotStore {
  type Dot = LamportClock
  type DotSet = Set[Dot]
  type DotFun[V] = Map[Dot, V]
  type DotMap[K, V] = Map[K, V]

  type CausalContext = VectorClock

  import scala.language.implicitConversions

  implicit def dotSetToVectorClock(dotSet: DotSet): VectorClock = {
    dotSet.foldLeft(VectorClock())((vc, dot) => vc.merged(dot))
  }

  def apply[D](implicit dotStore: DotStore[D]): DotStore[D] = dotStore

  implicit def DotSet: DotStore[DotSet] = new DotStore[DotSet] {
    override def dots(dotStore: DotSet): Set[Dot] = dotStore

    override def bottom: DotSet = Set.empty
  }

  implicit def DotFun[V: SemiLattice]: DotStore[DotFun[V]] = new DotStore[DotFun[V]] {
    override def dots(dotStore: DotFun[V]): Set[Dot] = dotStore.keySet

    override def bottom: DotFun[V] = Map.empty
  }

  implicit def dotMap[K, V: DotStore]: DotStore[DotMap[K, V]] = new DotStore[DotMap[K, V]] {
    override def dots(dotStore: DotMap[K, V]): Set[Dot] =
      dotStore.values.flatMap(DotStore[V].dots(_)).toSet

    override def bottom: DotMap[K, V] = Map.empty
  }
}