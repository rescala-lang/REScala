package rescala.deltacrdts

import rescala.deltacrdts.dotstores.DotStore._
import rescala.deltacrdts.dotstores.{Causal, Dot, DotStore}
import rescala.lattices.{IdUtil, Lattice}
import rescala.lattices.IdUtil.Id

case class AddWinsSet[A](current: Map[Id, Set[Dot]], past: Set[Dot], ids: Map[A, Id]) {
  //(updatesCurrent[Set[(id, dot)], knownPast[Set[dot]], newData[Set[(id,data)])
  // a delta always includes new (id,dot) pairs, the known causal context for the modified ids as well as the new data elements
  type TDelta = (Map[Id, Set[Dot]], Set[Dot], Set[(A, Id)])

  /**
    * Adding an element adds it to the current dot store as well as to the causal context (past).
    *
    * @param e the element to be added
    * @return
    */
  def add(e: A): TDelta = {
    val id = ids.getOrElse(e, IdUtil.genId())
    val dot = DotStore.next(id, past)
    // this is what the paper does:
    //    (Set((id, dot)), past.getOrElse(id, Set()) + dot, Set((id, e)))
    // this is sufficient in my opinion:
    // for adds we don't have to know (in the CC) conflicting adds or removes for this element because adds win anyway
    (Map(id -> Set(dot)), Set(dot), Set((e, id)))
  }

  /** Merging removes all elements the other side should known (based on the causal context),
    * but does not contain.
    * Thus, the delta for removal is the empty map,
    * with the dot of the removed element in the context. */
  def remove(e: A): TDelta = {
    ids.get(e) match {
      case None => throw new IllegalArgumentException(s"Cannot remove element $e since it is not in the set.")
      case Some(id) => (Map(), current(id).dots, Set())
    }
  }

  def clear: TDelta = {
    (Map(), current.dots, Set())
  }

  def toSet: Set[A] = ids.keySet.filter(d => current.keySet.contains(ids(d)))

  def contains(e: A): Boolean = toSet.contains(e)
}

trait CausalCRDT[TCausal, TDotStore, TDelta] {
  def apply(causal: Causal[TDotStore]): TCausal

  def dotStore(a: TCausal): TDotStore

  def causalContext(a: TCausal): Set[Dot]

  def merge(left: TCausal, right: TCausal)(implicit ev: DotStore[TDotStore]): TCausal = {
    def mkCausal(v: TCausal): Causal[TDotStore] = Causal(dotStore(v), causalContext(v))
    apply(DotStore[TDotStore].merge(mkCausal(left), mkCausal(right)))
  }
}

object AWS {
  private type Delta[A] = (Map[Id, Set[Dot]], Set[Dot], Set[(A, Id)])
  def applyΔ[T](crdt: AddWinsSet[T], delta: Delta[T]): AddWinsSet[T] = {
    val (deltaCurrent, deltaPast, deltaIdData) = delta
    AddWinsSet.addWinsSetLattice.merge(crdt, AddWinsSet(deltaCurrent, deltaPast, deltaIdData.toMap))
  }
}

object AddWinsSet {
  implicit def addWinsSetLattice[A]: Lattice[AddWinsSet[A]] = new Lattice[AddWinsSet[A]] {
    override def merge(left: AddWinsSet[A], right: AddWinsSet[A]): AddWinsSet[A] = {
      def mkCausal(v: AddWinsSet[A]): Causal[Map[Id, Set[Dot]]] = Causal(v.current, v.past)
      val causal  = DotStore.merge(mkCausal(left), mkCausal(right))
      val ids = left.ids ++ right.ids // also merge data->id maps of the two sets
      AddWinsSet(causal.store, causal.context, ids)
    }
  }
}

