package kofre.datatypes

import kofre.base.{Bottom, Lattice}
import kofre.datatypes.GrowOnlyList.Node
import kofre.datatypes.GrowOnlyList.Node.Elem
import kofre.dotted.HasDots
import kofre.syntax.OpsSyntaxHelper

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.infixOrderingOps

/** A GrowOnlyList is a Delta CRDT modeling a grow-only list where list elements can neither be removed nor modified.
  *
  * Concurrent inserts at the same index i are resolved by the timestamps of the insert operations: the later insert
  * will be at index i while the earlier insert will be pushed to index i+1.
  *
  * Note: GrowOnlyList is implemented as a linked list, thus the time needed to execute operations at the end of the list will
  * scale linearly with the length of the list. Similarly, toList always has to iterate the whole list, so for applications
  * that don't always need the whole list you should consider using toLazyList instead.
  */
case class GrowOnlyList[E](inner: Map[Node[LastWriterWins[E]], Elem[LastWriterWins[E]]])

object GrowOnlyList {
  enum Node[+E]:
    case Head
    case Elem(value: E)
  import Node.{Elem, Head}

  def empty[E]: GrowOnlyList[E] = GrowOnlyList(Map.empty)

  given bottomInstance[E]: Bottom[GrowOnlyList[E]]    = Bottom.derived
  given hasDots[E: HasDots]: HasDots[GrowOnlyList[E]] = HasDots.noDots

  given Lattice[E]: Lattice[GrowOnlyList[E]] =
    new Lattice[GrowOnlyList[E]] {
      override def lteq(
          left: GrowOnlyList[E],
          right: GrowOnlyList[E]
      ): Boolean =
        left.inner.keys.forall { k =>
          right.inner.get(k).contains(left.inner(k))
        } || super.lteq(left, right)

      /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
      override def decompose(state: GrowOnlyList[E]): Iterable[GrowOnlyList[E]] =
        state.inner.toList.map(edge => GrowOnlyList(Map(edge)))

      @tailrec
      private def insertEdge(
          state: GrowOnlyList[E],
          edge: (Node[LastWriterWins[E]], Elem[LastWriterWins[E]])
      ): GrowOnlyList[E] =
        edge match {
          case (l, r @ Elem(e1)) =>
            state.inner.get(l) match {
              case None => GrowOnlyList(state.inner + edge)
              case Some(next @ Elem(e2)) =>
                if e1.timestamp > e2.timestamp
                then GrowOnlyList(state.inner + edge + (r -> next))
                else
                  insertEdge(state, next -> r)
            }
        }

      @tailrec
      private def insertRec(
          left: GrowOnlyList[E],
          right: GrowOnlyList[E],
          current: Node[LastWriterWins[E]]
      ): GrowOnlyList[E] =
        right.inner.get(current) match {
          case None => left
          case Some(next) =>
            val leftMerged =
              if left.inner.contains(current) && left.inner.exists { case (_, r) => r == next }
              then left
              else insertEdge(left, (current, next))

            insertRec(leftMerged, right, next)
        }

      /** By assumption: associative, commutative, idempotent. */
      override def merge(
          left: GrowOnlyList[E],
          right: GrowOnlyList[E]
      ): GrowOnlyList[E] =
        (right.inner.keySet -- right.inner.values).foldLeft(left) { (state, startNode) =>
          insertRec(state, right, startNode)
        }
    }

  extension [C, E](container: C)
    def growOnlyList: syntax[C, E] = syntax(container)

  implicit class syntax[C, E](container: C)
      extends OpsSyntaxHelper[C, GrowOnlyList[E]](container) {

    @tailrec
    private def findNth(
        state: GrowOnlyList[E],
        current: Node[LastWriterWins[E]],
        i: Int
    ): Option[Node[LastWriterWins[E]]] = {
      if (i == 0) Some(current)
      else state.inner.get(current) match {
        case None       => None
        case Some(elem) => findNth(state, elem, i - 1)
      }
    }

    def read(using IsQuery)(i: Int): Option[E] =
      findNth(current, Head, i + 1).flatMap {
        case Head    => None
        case Elem(e) => Some(e.payload)
      }

    def toList(using IsQuery): List[E] =
      val state = current
      @tailrec
      def toListRec(current: Node[LastWriterWins[E]], acc: List[E]): List[E] =
        state.inner.get(current) match {
          case None                  => acc.reverse
          case Some(next @ Elem(tv)) => toListRec(next, tv.payload :: acc)
        }

      toListRec(Head, Nil)

    def toLazyList(using IsQuery): LazyList[E] =
      LazyList.unfold[E, Node[LastWriterWins[E]]](Head) { node =>
        current.inner.get(node) match {
          case None                  => None
          case Some(next @ Elem(tv)) => Some((tv.payload, next))
        }
      }

    def size(using IsQuery): Int = current.inner.size

    def insertGL(i: Int, e: E): Mutator = {
      GrowOnlyList(findNth(current, Head, i) match {
        case None => Map.empty
        case Some(pred) =>
          Map(pred -> Elem(LastWriterWins.now(e)))
      })
    }.mutator

    def insertAllGL(i: Int, elems: Iterable[E]): Mutator = {
      if (elems.isEmpty)
        GrowOnlyList.empty[E]
      else
        GrowOnlyList(findNth(current, Head, i) match {
          case None => Map.empty
          case Some(after) =>
            val order = elems.map(e => Elem(LastWriterWins.now(e)): Elem[LastWriterWins[E]])
            Map((List(after) ++ order.init) zip order: _*)
        })
    }.mutator

    @tailrec
    private def withoutRec(state: GrowOnlyList[E], current: Node[LastWriterWins[E]], elems: Set[E]): GrowOnlyList[E] =
      state.inner.get(current) match {
        case None => state
        case Some(next @ Elem(tv)) if elems.contains(tv.payload) =>
          val edgeRemoved = state.inner.get(next) match {
            case Some(nextnext) => state.inner.removed(current).removed(next) + (current -> nextnext)
            case None           => state.inner.removed(current).removed(next)
          }

          withoutRec(GrowOnlyList(edgeRemoved), current, elems)
        case Some(next) => withoutRec(state, next, elems)
      }

    def without(elems: Set[E])(using IsMutator): C = withoutRec(current, Head, elems).mutator
  }

}
