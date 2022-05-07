package kofre.decompose.interfaces

import kofre.decompose.*
import kofre.primitives.Epoche
import kofre.syntax.OpsSyntaxHelper

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** A GList is a Delta CRDT modeling a grow-only list where list elements can neither be removed nor modified.
  *
  * Concurrent inserts at the same index i are resolved by the timestamps of the insert operations: the later insert
  * will be at index i while the earlier insert will be pushed to index i+1.
  *
  * Note: GList is implemented as a linked list, thus the time needed to execute operations at the end of the list will
  * scale linearly with the length of the list. Similarly, toList always has to iterate the whole list, so for applications
  * that don't always need the whole list you should consider using toLazyList instead.
  */
object GListInterface {
  sealed trait GListNode[E]
  case class Head[E]()         extends GListNode[E]
  case class Elem[E](value: E) extends GListNode[E]

  type GList[E] = Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]]

  implicit def GListAsUIJDLattice[E]: DecomposeLattice[GList[E]] =
    new DecomposeLattice[Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]]] {
      override def lteq(
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

      override def empty: Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]] = Map.empty

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
      private def insertRec(left: GList[E], right: GList[E], current: GListNode[TimedVal[E]]): GList[E] =
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

  implicit class GListSyntax[C, E](container: C) extends OpsSyntaxHelper[C, GList[E]](container) {

    @tailrec
    private def findNth(state: GList[E], current: GListNode[TimedVal[E]], i: Int): Option[GListNode[TimedVal[E]]] = {
      if (i == 0) Some(current)
      else state.get(current) match {
        case None       => None
        case Some(elem) => findNth(state, elem, i - 1)
      }
    }

    def read(i: Int)(using QueryP): Option[E] =
      findNth(current, Head[TimedVal[E]](), i + 1).flatMap {
        case Head()  => None
        case Elem(e) => Some(e.value)
      }

    @tailrec
    private def toListRec(state: GList[E], current: GListNode[TimedVal[E]], acc: ListBuffer[E]): ListBuffer[E] =
      state.get(current) match {
        case None                  => acc
        case Some(next @ Elem(tv)) => toListRec(state, next, acc.append(tv.value))
      }

    def toList(using QueryP): List[E] =
      toListRec(current, Head[TimedVal[E]](), ListBuffer.empty[E]).toList

    def toLazyList(using QueryP):  LazyList[E] =
      LazyList.unfold[E, GListNode[TimedVal[E]]](Head[TimedVal[E]]()) { node =>
        current.get(node) match {
          case None                  => None
          case Some(next @ Elem(tv)) => Some((tv.value, next))
        }
      }

    def size(using QueryP): Int = current.size

    def insert(i: Int, e: E)(using MutationIDP): C = {
      findNth(current, Head[TimedVal[E]](), i) match {
        case None       => Map.empty
        case Some(pred) => Map(pred -> Elem(TimedVal(e, replicaID)))
      }
    }

    def insertAll(i: Int, elems: Iterable[E])(using MutationIDP): C = {
      if (elems.isEmpty)
        DecomposeLattice[GList[E]].empty
      else
        findNth(current, Head[TimedVal[E]](), i) match {
          case None => Map.empty
          case Some(after) =>
            val order = elems.map(e => Elem(TimedVal(e, replicaID)))
            Map((List(after) ++ order.init) zip order: _*)
        }
    }

    @tailrec
    private def withoutRec(state: GList[E], current: GListNode[TimedVal[E]], elems: Set[E]): GList[E] =
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

    def without(elems: Set[E])(using MutationP): C = withoutRec(current, Head[TimedVal[E]](), elems)
  }

}
