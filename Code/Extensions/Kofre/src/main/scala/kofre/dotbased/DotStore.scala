package kofre.dotbased

import kofre.Defs.Id
import kofre.Lattice
import kofre.causality.{CausalContext, Dot}

/** Dot stores provide a generic way to merge datastructures,
  * implemented on top of one of the provided dot stores.
  * See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
  *
  * A dot store seems to essentially be a lattice of different causal instances,
  * with some bonus operations. But not quite.
  */
trait DotStore[Store] {
  def dots(a: Store): CausalContext
  def empty: Store
}

object DotStore {

  def apply[A](implicit dotStore: DotStore[A]): dotStore.type = dotStore

  type DotSet       = CausalContext
  type DotFun[V]    = Map[Dot, V]
  type DotMap[K, V] = Map[K, V]

  // instances

  implicit def dotFunDotStore[V]: DotStore[DotFun[V]] = new DotStore[DotFun[V]] {
    override def empty: DotFun[V]                         = Map.empty
    override def dots(dotStore: DotFun[V]): CausalContext = CausalContext.fromSet(dotStore.keySet)
  }

  implicit val CausalContextDotStoreInstance: DotStore[CausalContext] =
    new DotStore[CausalContext] {
      override def dots(a: CausalContext): CausalContext = a
      override def empty: CausalContext                  = CausalContext.empty
    }

  implicit val DotSetInstance: DotStore[Set[Dot]] =
    new DotStore[Set[Dot]] {
      override def dots(a: Set[Dot]): CausalContext = CausalContext.fromSet(a)
      override def empty: Set[Dot]                  = Set.empty
    }

  implicit def DotMapInstance[Key, A: DotStore]: DotStore[Map[Key, A]] =
    new DotStore[Map[Key, A]] {
      override def dots(a: Map[Key, A]): CausalContext =
        a.valuesIterator.foldLeft(CausalContext.empty)((acc, v) => acc.union(DotStore[A].dots(v)))
      override def empty: Map[Key, A] = Map.empty
    }
}
