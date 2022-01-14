package rescala.extra.lattices.delta.interfaces

import rescala.extra.lattices.delta.CRDTInterface.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object GListInterface {
  sealed trait GListNode[E]
  case class Head[E]()         extends GListNode[E]
  case class Elem[E](value: E) extends GListNode[E]

  type State[E] = Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]]

  trait GListCompanion {
    type State[E] = GListInterface.State[E]

    implicit def GListAsUIJDLattice[E]: UIJDLattice[State[E]] = GListInterface.GListAsUIJDLattice[E]
  }

  implicit def GListAsUIJDLattice[E]: UIJDLattice[State[E]] =
    new UIJDLattice[Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]]] {
      override def leq(
          left: Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]],
          right: Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]]
      ): Boolean =
        left.keys.forall { k =>
          right.get(k).contains(left(k))
        }

      /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
      override def decompose(state: Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]])
          : Iterable[Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]]] =
        state.toList.map((edge: (GListNode[TimedVal[E]], Elem[TimedVal[E]])) => Map(edge))

      override def bottom: Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]] = Map.empty

      @tailrec
      private def insertEdge(
          state: Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]],
          edge: (GListNode[TimedVal[E]], Elem[TimedVal[E]])
      ): Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]] =
        edge match {
          case (l, r @ Elem(e1)) =>
            state.get(l) match {
              case None => state + edge
              case Some(next @ Elem(e2)) =>
                if (e1.laterThan(e2)) state + edge + (r -> next)
                else insertEdge(state, next -> r)
            }
        }

      @tailrec
      private def insertRec(left: State[E], right: State[E], current: GListNode[TimedVal[E]]): State[E] =
        right.get(current) match {
          case None => left
          case Some(next) =>
            val leftMerged =
              if (left.contains(current) && left.exists { case (_, r) => r == next })
                left
              else
                insertEdge(left, (current, next))

            insertRec(leftMerged, right, next)
        }

      /** By assumption: associative, commutative, idempotent. */
      override def merge(
          left: Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]],
          right: Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]]
      ): Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]] =
        (right.keySet -- right.values).foldLeft(left) { (state, startNode) =>
          insertRec(state, right, startNode)
        }
    }

  @tailrec
  private def findNth[E](state: State[E], current: GListNode[TimedVal[E]], i: Int): Option[GListNode[TimedVal[E]]] = {
    if (i == 0) Some(current)
    else state.get(current) match {
      case None       => None
      case Some(elem) => findNth(state, elem, i - 1)
    }
  }

  def read[E](i: Int): DeltaQuery[State[E], Option[E]] = state =>
    findNth(state, Head[TimedVal[E]](), i + 1).flatMap {
      case Head()  => None
      case Elem(e) => Some(e.value)
    }

  @tailrec
  private def toListRec[E](state: State[E], current: GListNode[TimedVal[E]], acc: ListBuffer[E]): ListBuffer[E] =
    state.get(current) match {
      case None                  => acc
      case Some(next @ Elem(tv)) => toListRec(state, next, acc.append(tv.value))
    }

  def toList[E]: DeltaQuery[State[E], List[E]] =
    state => toListRec(state, Head[TimedVal[E]](), ListBuffer.empty[E]).toList

  def toLazyList[E]: DeltaQuery[State[E], LazyList[E]] = state =>
    LazyList.unfold[E, GListNode[TimedVal[E]]](Head[TimedVal[E]]()) { node =>
      state.get(node) match {
        case None                  => None
        case Some(next @ Elem(tv)) => Some((tv.value, next))
      }
    }

  def size[E]: DeltaQuery[State[E], Int] = state => state.size

  def insert[E](i: Int, e: E): DeltaMutator[State[E]] = (replicaID, state) => {
    findNth(state, Head[TimedVal[E]](), i) match {
      case None       => Map.empty
      case Some(pred) => Map(pred -> Elem(TimedVal(e, replicaID)))
    }
  }

  def insertAll[E](i: Int, elems: Iterable[E]): DeltaMutator[State[E]] = (replicaID, state) => {
    if (elems.isEmpty)
      UIJDLattice[State[E]].bottom
    else
      findNth(state, Head[TimedVal[E]](), i) match {
        case None => Map.empty
        case Some(after) =>
          val order = elems.map(e => Elem(TimedVal(e, replicaID)))
          Map((List(after) ++ order.init) zip order: _*)
      }
  }

  @tailrec
  private def withoutRec[E](state: State[E], current: GListNode[TimedVal[E]], elems: Set[E]): State[E] =
    state.get(current) match {
      case None => state
      case Some(next @ Elem(tv)) if elems.contains(tv.value) =>
        val edgeRemoved = state.get(next) match {
          case Some(nextnext) => state.removed(current).removed(next) + (current -> nextnext)
          case None           => state.removed(current).removed(next)
        }

        withoutRec(edgeRemoved, current, elems)
      case Some(next) => withoutRec(state, next, elems)
    }

  def without[E](state: State[E], elems: Set[E]): State[E] = withoutRec(state, Head[TimedVal[E]](), elems)
}

/** A GList is a Delta CRDT modeling a grow-only list where list elements can neither be removed nor modified.
  *
  * Concurrent inserts at the same index i are resolved by the timestamps of the insert operations: the later insert
  * will be at index i while the earlier insert will be pushed to index i+1.
  *
  * Note: GList is implemented as a linked list, thus the time needed to execute operations at the end of the list will
  * scale linearly with the length of the list. Similarly, toList always has to iterate the whole list, so for applications
  * that don't always need the whole list you should consider using toLazyList instead.
  */
abstract class GListInterface[E, Wrapper] extends CRDTInterface[GListInterface.State[E], Wrapper] {
  def read(i: Int): Option[E] = query(GListInterface.read(i))

  def toList: List[E] = query(GListInterface.toList)

  def toLazyList: LazyList[E] = query(GListInterface.toLazyList)

  def size: Int = query(GListInterface.size)

  def insert(i: Int, e: E): Wrapper = mutate(GListInterface.insert(i, e))

  def insertAll(i: Int, elems: Iterable[E]): Wrapper = mutate(GListInterface.insertAll(i, elems))
}
