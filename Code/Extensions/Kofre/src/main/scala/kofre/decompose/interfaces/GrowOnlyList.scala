package kofre.decompose.interfaces

import kofre.base.{Bottom, DecomposeLattice}
import kofre.decompose.*
import kofre.datatypes.{Epoche, TimedVal}
import kofre.dotted.DottedDecompose
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** A GrowOnlyList is a Delta CRDT modeling a grow-only list where list elements can neither be removed nor modified.
  *
  * Concurrent inserts at the same index i are resolved by the timestamps of the insert operations: the later insert
  * will be at index i while the earlier insert will be pushed to index i+1.
  *
  * Note: GrowOnlyList is implemented as a linked list, thus the time needed to execute operations at the end of the list will
  * scale linearly with the length of the list. Similarly, toList always has to iterate the whole list, so for applications
  * that don't always need the whole list you should consider using toLazyList instead.
  */
object GrowOnlyList {
  sealed trait GListNode[+E]
  case class GListHead()            extends GListNode[Nothing]
  case class GListElem[E](value: E) extends GListNode[E]

  type GrowOnlyList[E] = Map[GListNode[TimedVal[E]], GListElem[TimedVal[E]]]

  def empty[E]: GrowOnlyList[E] = Map.empty

  given contextDecompose[E]: DottedDecompose[GrowOnlyList[E]] = DottedDecompose.liftDecomposeLattice

  implicit def GListAsUIJDLattice[E]: DecomposeLattice[GrowOnlyList[E]] =
    new DecomposeLattice[Map[GListNode[TimedVal[E]], GListElem[TimedVal[E]]]] {
      override def lteq(
          left: Map[GListNode[TimedVal[E]], GListElem[TimedVal[E]]],
          right: Map[GListNode[TimedVal[E]], GListElem[TimedVal[E]]]
      ): Boolean =
        left.keys.forall { k =>
          right.get(k).contains(left(k))
        }

      /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
      override def decompose(state: Map[GListNode[TimedVal[E]], GListElem[TimedVal[E]]])
          : Iterable[Map[GListNode[TimedVal[E]], GListElem[TimedVal[E]]]] =
        state.toList.map((edge: (GListNode[TimedVal[E]], GListElem[TimedVal[E]])) => Map(edge))

      @tailrec
      private def insertEdge(
          state: Map[GListNode[TimedVal[E]], GListElem[TimedVal[E]]],
          edge: (GListNode[TimedVal[E]], GListElem[TimedVal[E]])
      ): Map[GListNode[TimedVal[E]], GListElem[TimedVal[E]]] =
        edge match {
          case (l, r @ GListElem(e1)) =>
            state.get(l) match {
              case None => state + edge
              case Some(next @ GListElem(e2)) =>
                if (e1.laterThan(e2)) state + edge + (r -> next)
                else insertEdge(state, next -> r)
            }
        }

      @tailrec
      private def insertRec(left: GrowOnlyList[E], right: GrowOnlyList[E], current: GListNode[TimedVal[E]]): GrowOnlyList[E] =
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
          left: Map[GListNode[TimedVal[E]], GListElem[TimedVal[E]]],
          right: Map[GListNode[TimedVal[E]], GListElem[TimedVal[E]]]
      ): Map[GListNode[TimedVal[E]], GListElem[TimedVal[E]]] =
        (right.keySet -- right.values).foldLeft(left) { (state, startNode) =>
          insertRec(state, right, startNode)
        }
    }

  implicit class GListSyntax[C, E](container: C)(using ArdtOpsContains[C, GrowOnlyList[E]])
      extends OpsSyntaxHelper[C, GrowOnlyList[E]](container) {

    @tailrec
    private def findNth(state: GrowOnlyList[E], current: GListNode[TimedVal[E]], i: Int): Option[GListNode[TimedVal[E]]] = {
      if (i == 0) Some(current)
      else state.get(current) match {
        case None       => None
        case Some(elem) => findNth(state, elem, i - 1)
      }
    }

    def read(i: Int)(using QueryP): Option[E] =
      findNth(current, GListHead(), i + 1).flatMap {
        case GListHead()  => None
        case GListElem(e) => Some(e.value)
      }

    @tailrec
    private def toListRec(state: GrowOnlyList[E], current: GListNode[TimedVal[E]], acc: ListBuffer[E]): ListBuffer[E] =
      state.get(current) match {
        case None                       => acc
        case Some(next @ GListElem(tv)) => toListRec(state, next, acc.append(tv.value))
      }

    def toList(using QueryP): List[E] =
      toListRec(current, GListHead(), ListBuffer.empty[E]).toList

    def toLazyList(using QueryP): LazyList[E] =
      LazyList.unfold[E, GListNode[TimedVal[E]]](GListHead()) { node =>
        current.get(node) match {
          case None                       => None
          case Some(next @ GListElem(tv)) => Some((tv.value, next))
        }
      }

    def size(using QueryP): Int = current.size

    def insert(i: Int, e: E)(using MutationIdP): C = {
      findNth(current, GListHead(), i) match {
        case None       => Map.empty
        case Some(pred) => Map(pred -> GListElem(TimedVal(e, replicaID)))
      }
    }.mutator

    def insertAll(i: Int, elems: Iterable[E])(using MutationP, IdentifierP): C = {
      if (elems.isEmpty)
        GrowOnlyList.empty[E]
      else
        findNth(current, GListHead(), i) match {
          case None => Map.empty
          case Some(after) =>
            val order = elems.map(e => GListElem(TimedVal(e, replicaID)))
            Map((List(after) ++ order.init) zip order: _*)
        }
    }.mutator

    @tailrec
    private def withoutRec(state: GrowOnlyList[E], current: GListNode[TimedVal[E]], elems: Set[E]): GrowOnlyList[E] =
      state.get(current) match {
        case None => state
        case Some(next @ GListElem(tv)) if elems.contains(tv.value) =>
          val edgeRemoved = state.get(next) match {
            case Some(nextnext) => state.removed(current).removed(next) + (current -> nextnext)
            case None           => state.removed(current).removed(next)
          }

          withoutRec(edgeRemoved, current, elems)
        case Some(next) => withoutRec(state, next, elems)
      }

    def without(elems: Set[E])(using MutationP): C = withoutRec(current, GListHead(), elems).mutator
  }

}
