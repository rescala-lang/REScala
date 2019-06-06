package rescala.deltacrdts

import rescala.deltacrdts.dotstores.DotStore._
import rescala.deltacrdts.dotstores.{Causal, Dot, DotStore}
import rescala.lattices.IdUtil
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

trait CausalCRDT[TCausal, TDotStore, TDelta] extends DeltaCRDT[TCausal, TDelta] {
  def apply(causal: Causal[TDotStore]): TCausal

  def dotStore(a: TCausal): TDotStore

  def causalContext(a: TCausal): Set[Dot]

  def merge(left: TCausal, right: TCausal)(implicit ev: DotStore[TDotStore]): TCausal = {
    def mkCausal(v: TCausal): Causal[TDotStore] = Causal(dotStore(v), causalContext(v))
    apply(DotStore[TDotStore].merge(mkCausal(left), mkCausal(right)))
  }
}

object CausalCRDT {

  implicit class CausalCRDTOps[A](caller: A) {
    def merge[T, D](right: A)(implicit causalCRDT: CausalCRDT[A, T, D], dotStore: DotStore[T]): A = {
      causalCRDT.merge(caller, right)
    }
  }

  implicit def addWinsSetCRDT[A]: CausalCRDT[AddWinsSet[A], Map[Id, Set[Dot]], (Map[Id, Set[Dot]], Set[Dot], Set[(A, Id)])] =
    new CausalCRDT[AddWinsSet[A], Map[Id, Set[Dot]], (Map[Id, Set[Dot]], Set[Dot], Set[(A, Id)])] {
      override def dotStore(addWinsSet: AddWinsSet[A]): Map[Id, Set[Dot]] = addWinsSet.current

      override def causalContext(addWinsSet: AddWinsSet[A]): Set[Dot] = addWinsSet.past

      override def merge(left: AddWinsSet[A], right: AddWinsSet[A])(implicit ev: DotStore[Map[Id, Set[Dot]]]): AddWinsSet[A] = {
        val causal: AddWinsSet[A] = super.merge(left, right)
        val current = dotStore(causal)
        val past = causalContext(causal)

        val ids = left.ids ++ right.ids // also merge data->id maps of the two sets
        AddWinsSet(current, past, ids)
      }

      override def apply(causal: Causal[Map[Id, Set[Dot]]]): AddWinsSet[A] =
        // TODO: what to do when I have the dotStore, the causalContext but not the actual values for the ids?
        AddWinsSet(causal.store, causal.context, Map())
      override def applyΔ(crdt: AddWinsSet[A], delta: (Map[Id, Set[Dot]], Set[Dot], Set[(A, Id)])): AddWinsSet[A] = {
        val (deltaCurrent, deltaPast, deltaIdData) = delta
        merge(crdt, AddWinsSet(deltaCurrent, deltaPast, deltaIdData.toMap))
      }
    }
}

