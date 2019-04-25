package rescala.deltacrdts

import rescala.deltacrdts.dotstores.DotStore._
import rescala.deltacrdts.dotstores.{Dot, DotStore}
import rescala.lattices.IdUtil
import rescala.lattices.IdUtil.Id

case class AddWinsSet[A](current: Map[Id, Set[Dot]], past: Set[Dot], ids: Map[A, Id]) {
  //(updatesCurrent[Set[(id, dot)], knownPast[Set[dot]], newData[Set[(id,data)])
  // a delta always includes new (id,dot) pairs, the known causal context for the modified ids as well as the new data elements
  type delta = (Map[Id, Set[Dot]], Set[Dot], Set[(A, Id)])

  /**
    * Adding an element adds it to the current dot store as well as to the causal context (past).
    *
    * @param e the element to be added
    * @return
    */
  def add(e: A): delta = {
    val id = ids.getOrElse(e, IdUtil.genId())
    val dot = DotStore.next(id, past)
    // this is what the paper does:
    //    (Set((id, dot)), past.getOrElse(id, Set()) + dot, Set((id, e)))
    // this is sufficient in my opinion:
    // for adds we don't have to know (in the CC) conflicting adds or removes for this element because adds win anyway
    (Map(id -> Set(dot)), Set(dot), Set((e, id)))
  }

  /**
    * Removing an element adds its dots to the causal context.
    *
    * @param e the element to be removed
    * @return
    */
  def remove(e: A): delta = {
    ids.get(e) match {
      case None => throw new IllegalArgumentException(s"Cannot remove element $e since it is not in the set.")
      case Some(id) => (Map(), current(id).dots, Set())
    }
  }

  def clear: delta = {
    (Map(), current.dots, Set())
  }

  def toSet: Set[A] = ids.keySet.filter(d => current.keySet.contains(ids(d)))

  def contains(e: A): Boolean = toSet.contains(e)
}

/**
  *
  * @tparam A type of the CausalCRDT
  * @tparam T type of the DotStore
  * @tparam D type of the Deltas
  */
trait CausalCRDT[A, T, D] extends DeltaCRDT[A, D] {
  def apply(dotStore: T, causalContext: Set[Dot]): A

  def dotStore(a: A): T

  def causalContext(a: A): Set[Dot]

  def merge(left: A, right: A)(implicit ev: DotStore[T]): A = {
    val (d, c) = DotStore.merge(dotStore(left), causalContext(left), dotStore(right), causalContext(right))
    apply(d, c)
  }
}

object CausalCRDT {
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

      override def apply(store: Map[Id, Set[Dot]], causalContext: Set[Dot]): AddWinsSet[A] = AddWinsSet(store, causalContext, Map()) // TODO: what to do when I have the dotStore, the causalContext but not the actual values for the ids?

      override def applyΔ(crdt: AddWinsSet[A], delta: (Map[Id, Set[Dot]], Set[Dot], Set[(A, Id)])): AddWinsSet[A] = {
        val (deltaCurrent, deltaPast, deltaIdData) = delta
        merge(crdt, AddWinsSet(deltaCurrent, deltaPast, deltaIdData.toMap))
      }
    }
}

