package rescala.extra.lattices.sets

import rescala.extra.lattices.IdUtil.Id
import rescala.extra.lattices.Lattice
import rescala.extra.lattices.dotstores.Context

case class AddWinsSetO[A](store: Map[A, Context], context: Context) {

  def add(element: A, replicaID: Id): AddWinsSetO[A] = Lattice.merge(this, addΔ(element, replicaID))

  /** Adding an element adds it to the current dot store as well as to the causal context (past). */
  def addΔ(element: A, replicaID: Id): AddWinsSetO[A] = {
    val time     = context.nextTime(replicaID)
    val singular = Context.single(replicaID, time)
    AddWinsSetO(Map(element -> singular), store.get(element).fold(singular)(_.add(replicaID, time)))

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
  def removeΔ(e: A): AddWinsSetO[A] = AddWinsSetO[A](Map.empty, store.getOrElse(e, Context.empty))
  def remove(e: A): AddWinsSetO[A]  = Lattice.merge(this, removeΔ(e))

  def toSet: Set[A] = store.keySet

  def clear: AddWinsSetO[A] = AddWinsSetO[A](Map.empty, context)

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

object AddWinsSetO {

  def empty[A]: AddWinsSetO[A] = AddWinsSetO[A](Map.empty, Context.empty)

  def latticeAddWinsSet[A]: Lattice[AddWinsSetO[A]] =
    new Lattice[AddWinsSetO[A]] {

      override def merge(left: AddWinsSetO[A], right: AddWinsSetO[A]): AddWinsSetO[A] = {

        // The new store is everything both sides have seen and everything that is new.
        // If something is missing from the store (but in the context) it has been deleted.
        val newStore = (left.store.keys ++ right.store.keys).toSet.map { (keyValue: A) =>
          val leftDots: Context  = left.store.getOrElse(keyValue, Context.empty)
          val rightDots: Context = right.store.getOrElse(keyValue, Context.empty)

          val common: Context      = leftDots.intersect(rightDots)
          val leftNew: Context     = leftDots diff right.context
          val rightNew: Context    = rightDots diff left.context
          val newElements: Context = Lattice.merge(leftNew, rightNew)
          val fullNew: Context     = Lattice.merge(common, newElements)

          (keyValue, fullNew)
        }.filter { _._2 != Context.empty }.toMap

        // the merged state has seen everything from both sides
        val newContext = Lattice.merge(left.context, right.context)
        AddWinsSetO(newStore, newContext)
      }
    }

  implicit def latticeAddWinsSetPerfOpt[A]: Lattice[AddWinsSetO[A]] =
    new Lattice[AddWinsSetO[A]] {

      override def merge(left: AddWinsSetO[A], right: AddWinsSetO[A]): AddWinsSetO[A] = {

        val leftChanges = left.store.map {
          case (value, context) =>
            val otherDots      = right.store.getOrElse(value, Context.empty)
            val intersection   = context.intersect(otherDots)
            val additions      = context.diff(right.context)
            val otherAdditions = otherDots.diff(left.context)
            val newContext     = Lattice.merge(Lattice.merge(intersection, additions), otherAdditions)
            if (newContext == context) None
            else Some(value -> newContext)
        }

        val rightAdditions = right.store.collect {
          case (value, context) if !left.store.contains(value) =>
            val rightAdditions = context.diff(left.context)
            Some(value -> rightAdditions)
        }

        val newStore = (leftChanges ++ rightAdditions)
          .collect { case Some(pair) => pair }
          .foldLeft(left.store) {
            case (store, (value, context)) =>
              store.updated(value, context)
          }

        // the merged state has seen everything from both sides
        val newContext = Lattice.merge(left.context, right.context)
        AddWinsSetO(newStore, newContext)
      }
    }

}
