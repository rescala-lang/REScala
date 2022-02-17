package kofre.dotbased

import kofre.IdUtil.Id
import kofre.Lattice
import kofre.primitives.Dot

case class Causal[A](store: A, context: Set[Dot])







/** Dot stores provide a generic way to merge datastructures,
  * implemented on top of one of the provided dot stores.
  */
trait DotStoreLattice[Store] {
  def add(a: Store, d: Dot): Store

  def dots(a: Store): Set[Dot]

  def compress(a: Store): Store

  def empty: Store

  /** The new element contains all the dots that are either
    * contained in both dotstores or contained in one of the dotstores but not in the causal context (history) of the
    * other one.
    */
  def merge(left: Causal[Store], right: Causal[Store]): Causal[Store]
}

object DotStoreLattice {
  def next[A: DotStoreLattice](id: Id, c: A): Dot = {
    val dotsWithId = DotStoreLattice[A].dots(c).filter(_.replicaId == id)
    val maxCount   = if (dotsWithId.isEmpty) 0 else dotsWithId.map(_.counter).max
    Dot(id, maxCount + 1)
  }

  def merge[A: DotStoreLattice](left: Causal[A], right: Causal[A]): Causal[A] = {
    DotStoreLattice[A].merge(left, right)
  }

  def apply[A](implicit dotStore: DotStoreLattice[A]): dotStore.type = dotStore

  // instances

  implicit def DotSetInstance: DotStoreLattice[Set[Dot]] =
    new DotStoreLattice[Set[Dot]] {
      type Store = Set[Dot]

      override def add(a: Store, d: Dot): Store = a + d

      override def dots(a: Store): Store = a

      /** Only keeps the highest element of each dot subsequence in the set. */
      // TODO: how do we know where subsequences started?
      // TODO: this most likely only works with causal delivery of things, which we may not have
      override def compress(a: Store): Store = a.filter(d => !a.contains(Dot(d.replicaId, d.counter + 1)))

      override def empty: Store = Set.empty

      override def merge(left: Causal[Store], right: Causal[Store]): Causal[Store] = {
        val common      = left.store intersect right.store
        val newElements = (left.store diff right.context) union (right.store diff left.context)
        Causal(common union newElements, left.context union right.context)
      }
    }

  implicit def DotMapInstance[Key, A](implicit dsl: DotStoreLattice[A]): DotStoreLattice[Map[Key, A]] =
    new DotStoreLattice[Map[Key, A]] {
      type Store = Map[Key, A]

      override def add(a: Store, d: Dot): Store = a.mapValues(v => dsl.add(v, d)).toMap: @scala.annotation.nowarn()

      override def dots(a: Store): Set[Dot] = a.valuesIterator.flatMap(dsl.dots).toSet

      override def compress(a: Store): Store = a.mapValues(dsl.compress).toMap: @scala.annotation.nowarn()

      override def empty: Store = Map.empty

      override def merge(left: Causal[Store], right: Causal[Store]): Causal[Store] = {

        val empty = DotStoreLattice[A].empty

        // The new store is everything both sides have seen and everything that is new.
        // If something is missing from the store (but in the context) it has been deleted.
        val newStore: Store = (left.store.keySet union right.store.keySet).map { id =>
          val value = DotStoreLattice[A].merge(
            Causal(left.store.getOrElse(id, empty), left.context),
            Causal(right.store.getOrElse(id, empty), right.context)
          )
          (id, value.store)
        }.filter { _._2 != empty }
          .toMap

        // the merged state has seen everything from both sides
        val newContext = left.context union right.context
        Causal(newStore, newContext)
      }
    }
}
