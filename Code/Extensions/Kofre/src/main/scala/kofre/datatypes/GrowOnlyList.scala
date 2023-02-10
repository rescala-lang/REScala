package kofre.datatypes

import kofre.base.{Bottom, Lattice}
import kofre.datatypes.GrowOnlyList.Node
import kofre.datatypes.{Epoche}
import kofre.dotted.DottedLattice
import kofre.syntax.{OpsSyntaxHelper}
import kofre.datatypes.LastWriterWins.TimedVal

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
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
case class GrowOnlyList[E](innerContents: GrowOnlyList.Repr[E]) {
  export innerContents.*
}

object GrowOnlyList {
  enum Node[+E]:
    case Head()            extends Node[Nothing]
    case Elem[E](value: E) extends Node[E]
  import Node.{Elem, Head}

  type Repr[E] = Map[
    Node[TimedVal[E]],
    Elem[TimedVal[E]]
  ]

  def empty[E]: GrowOnlyList[E] = GrowOnlyList(Map.empty)

  given bottomInstance[E]: Bottom[GrowOnlyList[E]] = Bottom.derived
  given contextDecompose[E]: DottedLattice[GrowOnlyList[E]] =
    DottedLattice.liftLattice

  given Lattice[E]: Lattice[GrowOnlyList[E]] =
    new Lattice[GrowOnlyList[E]] {
      override def lteq(
          left: GrowOnlyList[E],
          right: GrowOnlyList[E]
      ): Boolean =
        left.keys.forall { k =>
          right.get(k).contains(left(k))
        }

      /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
      override def decompose(state: GrowOnlyList[E]): Iterable[GrowOnlyList[E]] =
        state.toList.map((edge: (Node[TimedVal[E]], Elem[TimedVal[E]])) => GrowOnlyList(Map(edge)))

      @tailrec
      private def insertEdge(
          state: GrowOnlyList[E],
          edge: (Node[TimedVal[E]], Elem[TimedVal[E]])
      ): GrowOnlyList[E] =
        edge match {
          case (l, r @ Elem(e1)) =>
            state.get(l) match {
              case None => GrowOnlyList(state.innerContents + edge)
              case Some(next @ Elem(e2)) =>
                if (e1.timestamp > e2.timestamp) GrowOnlyList(state.innerContents + edge + (r -> next))
                else insertEdge(state, next                                                   -> r)
            }
        }

      @tailrec
      private def insertRec(
          left: GrowOnlyList[E],
          right: GrowOnlyList[E],
          current: Node[TimedVal[E]]
      ): GrowOnlyList[E] =
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
          left: GrowOnlyList[E],
          right: GrowOnlyList[E]
      ): GrowOnlyList[E] =
        (right.keySet -- right.values).foldLeft(left) { (state, startNode) =>
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
        current: Node[TimedVal[E]],
        i: Int
    ): Option[Node[TimedVal[E]]] = {
      if (i == 0) Some(current)
      else state.get(current) match {
        case None       => None
        case Some(elem) => findNth(state, elem, i - 1)
      }
    }

    def read(using PermQuery)(i: Int): Option[E] =
      findNth(current, Head(), i + 1).flatMap {
        case Head()  => None
        case Elem(e) => Some(e.payload)
      }

    @tailrec
    private def toListRec(state: GrowOnlyList[E], current: Node[TimedVal[E]], acc: ListBuffer[E]): ListBuffer[E] =
      state.get(current) match {
        case None                  => acc
        case Some(next @ Elem(tv)) => toListRec(state, next, acc.append(tv.payload))
      }

    def toList(using PermQuery): List[E] =
      toListRec(current, Head(), ListBuffer.empty[E]).toList

    def toLazyList(using PermQuery): LazyList[E] =
      LazyList.unfold[E, Node[TimedVal[E]]](Head()) { node =>
        current.get(node) match {
          case None                  => None
          case Some(next @ Elem(tv)) => Some((tv.payload, next))
        }
      }

    def size(using PermQuery): Int = current.size

    def insertGL(i: Int, e: E): IdMut[C] = {
      GrowOnlyList(findNth(current, Head(), i) match {
        case None       => Map.empty
        case Some(pred) => Map(pred -> Elem(LastWriterWins.now(e, replicaId)))
      })
    }.mutator

    def insertAllGL(using PermMutate, ReplicaId)(i: Int, elems: Iterable[E]): C = {
      if (elems.isEmpty)
        GrowOnlyList.empty[E]
      else
        GrowOnlyList(findNth(current, Head(), i) match {
          case None => Map.empty
          case Some(after) =>
            val order = elems.map(e => Elem(LastWriterWins.now(e, replicaId)): Elem[TimedVal[E]])
            Map((List(after) ++ order.init) zip order: _*)
        })
    }.mutator

    @tailrec
    private def withoutRec(state: GrowOnlyList[E], current: Node[TimedVal[E]], elems: Set[E]): GrowOnlyList[E] =
      state.get(current) match {
        case None => state
        case Some(next @ Elem(tv)) if elems.contains(tv.payload) =>
          val edgeRemoved = state.get(next) match {
            case Some(nextnext) => state.removed(current).removed(next) + (current -> nextnext)
            case None           => state.removed(current).removed(next)
          }

          withoutRec(GrowOnlyList(edgeRemoved), current, elems)
        case Some(next) => withoutRec(state, next, elems)
      }

    def without(elems: Set[E])(using PermMutate): C = withoutRec(current, Head(), elems).mutator
  }

}
