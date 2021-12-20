package kofre.sets

import kofre.IdUtil.Id
import kofre.dotstores.DotStoreLattice._
import kofre.dotstores.{Causal, Dot, DotStoreLattice}
import kofre.{IdUtil, Lattice}

case class AddWinsSet[A](store: Map[A, Set[Dot]], context: Set[Dot]) {
  // (updatesCurrent[Set[(id, dot)], knownPast[Set[dot]], newData[Set[(id,data)])
  // a delta always includes new (id,dot) pairs, the known causal context for the modified ids as well as the new data elements

  /** Adds a value conceptually from a new random replica */
  // TODO: this … is probably not a good idea
  def addRandom(e: A): AddWinsSet[A] = {
    val id = IdUtil.genId()
    addΔ(e, id)
  }

  def add(element: A, replicaID: Id): AddWinsSet[A] = Lattice.merge(this, addΔ(element, replicaID))

  /** Adding an element adds it to the current dot store as well as to the causal context (past). */
  def addΔ(element: A, replicaID: Id): AddWinsSet[A] = {
    val dot     = DotStoreLattice.next(replicaID, context)
    val onlyDot = Set(dot)
    AddWinsSet(Map(element -> onlyDot), store.get(element).fold(onlyDot)(_ + dot))

    // TODO: potential optimization
    // this is what the paper does:
    //    (Set((id, dot)), past.getOrElse(id, Set()) + dot, Set((id, e)))
    // this is sufficient in my opinion:
    // for adds we don't have to know (in the CC) conflicting adds or removes for this element because adds win anyway
    // AddWinsSet(Map(id -> Set(dot)), Set(dot), Map(e -> id))

  }

  /** Merging removes all elements the other side should known (based on the causal context),
    * but does not contain.
    * Thus, the delta for removal is the empty map,
    * with the dot of the removed element in the context.
    */
  def removeΔ(e: A): AddWinsSet[A] = AddWinsSet[A](Map.empty, store.getOrElse(e, Set.empty))

  def remove(element: A): AddWinsSet[A] = Lattice.merge(this, removeΔ(element))

  def clear: AddWinsSet[A] = AddWinsSet[A](Map(), DotStoreLattice[Map[A, Set[Dot]]].dots(store))

  def toSet: Set[A] = store.keySet

  def contains(e: A): Boolean = store.contains(e)

}

//trait CausalCRDT[TCausal, TDotStore] {
//  def apply(causal: Causal[TDotStore]): TCausal
//
//  def dotStore(a: TCausal): TDotStore
//
//  def causalContext(a: TCausal): Set[Dot]
//
//  def merge(left: TCausal, right: TCausal)(implicit ev: DotStore[TDotStore]): TCausal = {
//    def mkCausal(v: TCausal): Causal[TDotStore] = Causal(dotStore(v), causalContext(v))
//    apply(DotStore[TDotStore].merge(mkCausal(left), mkCausal(right)))
//  }
//}

object AddWinsSet {

  def empty[A]: AddWinsSet[A] = AddWinsSet[A](Map.empty[A, Set[Dot]], Set.empty[Dot])

  /* AddWinsSet is isomorphic to the corresponding Causal */

  implicit def toCausal[A](addWinsSet: AddWinsSet[A]): Causal[Map[A, Set[Dot]]] =
    Causal(addWinsSet.store, addWinsSet.context)
  implicit def fromCausal[A](causal: Causal[Map[A, Set[Dot]]]): AddWinsSet[A] =
    AddWinsSet(causal.store, causal.context)

  implicit def addWinsSetLattice[A]: Lattice[AddWinsSet[A]] =
    new Lattice[AddWinsSet[A]] {
      override def merge(left: AddWinsSet[A], right: AddWinsSet[A]): AddWinsSet[A] =
        DotStoreLattice.DotMapInstance[A, Set[Dot]].merge(left, right)
    }

}
