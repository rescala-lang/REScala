package kofre.decompose.interfaces

import kofre.base.DecomposeLattice
import kofre.causality.{CausalContext, Dot}
import kofre.decompose.*
import kofre.syntax.{PermIdMutate, ArdtOpsContains, PermMutate, OpsSyntaxHelper}
import kofre.contextual.ContextDecompose.{DotFun, DotPair}
import kofre.decompose.interfaces.GListInterface.{GListAsUIJDLattice, GListSyntax}
import kofre.contextual.{WithContext, ContextDecompose}
import kofre.decompose.interfaces.RCounterInterface.RCounter
import kofre.predef.Epoche
import kofre.syntax.PermIdMutate.withID

/** An RGA (Replicated Growable Array) is a Delta CRDT modeling a list.
  *
  * When two values are concurrently inserted at an index i, the value of the insert operation with the later timestamp
  * will be at index i while the earlier inserted value will be pushed to index i+1. When an element is subject to two
  * concurrent updates, the later update overwrites the earlier update. If an element is concurrently updated and deleted,
  * the element will simply be deleted, ignoring the update.
  *
  * Note that RGAs are implemented as linked lists, thus the time needed to execute operations toward the end of the list
  * will scale linearly with the size of the list.
  *
  * To correctly handle concurrent remote inserts next to elements that were deleted locally, the RGA implementation internally
  * keeps deleted elements as hidden tombstones in the list. Since many tombstones will slow down the operations on this
  * data structure, purgeTombstones should be executed periodically to remove tombstones from the list. Note however that
  * this invalidates any concurrent insert operations. Ideally, purgeTombstones should only be called in downtime periods
  * and only by privileged replicas.
  *
  * This implementation was modeled after the RGA proposed by Roh et al. in "Replicated abstract data types: Building blocks
  * for collaborative applications", see [[https://www.sciencedirect.com/science/article/pii/S0743731510002716?casa_token=lQaLin7aEvcAAAAA:Esc3h3WvkFHUcvhalTPPvV5HbJge91D4-2jyKiSlz8GBDjx31l4xvfH8DIstmQ973PVi46ckXHg here]]
  */

object RGAInterface {

  sealed trait RGANode[A]
  case class Alive[A](v: TimedVal[A]) extends RGANode[A]
  case class Dead[A]()                extends RGANode[A]

  object RGANode {
    implicit def RGANodeAsUIJDLattice[A]: DecomposeLattice[RGANode[A]] = new DecomposeLattice[RGANode[A]] {
      override def lteq(left: RGANode[A], right: RGANode[A]): Boolean = (left, right) match {
        case (Dead(), _)            => false
        case (_, Dead())            => true
        case (Alive(lv), Alive(rv)) => rv.laterThan(lv)
      }

      /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
      override def decompose(state: RGANode[A]): Iterable[RGANode[A]] = List(state)

      override def empty: RGANode[A] = throw new UnsupportedOperationException("RGANode does not have a bottom value")

      /** By assumption: associative, commutative, idempotent. */
      override def merge(left: RGANode[A], right: RGANode[A]): RGANode[A] = (left, right) match {
        case (Alive(lv), Alive(rv)) => Alive(DecomposeLattice[TimedVal[A]].merge(lv, rv))
        case _                      => Dead()
      }
    }
  }

  type RGAEmbed[E] = (Epoche[GListInterface.GList[Dot]], Map[Dot, RGANode[E]])
  type RGA[E] = WithContext[RGAEmbed[E]]

  trait RGACompanion {
    type State[E]    = RGAInterface.RGA[E]
    type Embedded[E] = (Epoche[GListInterface.GList[Dot]], Map[Dot, RGANode[E]])

    implicit val ForcedWriteAsUIJDLattice: DecomposeLattice[Epoche[GListInterface.GList[Dot]]] =
      Epoche.epocheAsUIJDLattice[GListInterface.GList[Dot]]
  }

  private class DeltaStateFactory[E] {
    given ContextDecompose[Epoche[GListInterface.GList[Dot]]] = ContextDecompose.UIJDLatticeAsDecomposableDotStore
    val bottom: RGA[E] = WithContext.empty

    def make(
        epoche: Epoche[GListInterface.GList[Dot]] = bottom.store._1,
        df: Map[Dot, RGANode[E]] = bottom.store._2,
        cc: CausalContext = bottom.context
    ): RGA[E] = WithContext((epoche, df), cc)
  }

  private def deltaState[E]: DeltaStateFactory[E] = new DeltaStateFactory[E]

  implicit class RGASyntax[C, E](container: C)(using ArdtOpsContains[C, RGA[E]]) extends OpsSyntaxHelper[C, RGA[E]](container) {

    def read(i: Int)(using QueryP): Option[E] = {
      val (fw, df) = current.store
      fw.value.toLazyList.map(df).collect {
        case Alive(tv) => tv.value
      }.lift(i)
    }

    def size(using QueryP): Int = {
      val (_, df) = current.store
      df.values.count {
        case Dead()   => false
        case Alive(_) => true
      }
    }

    def toList(using QueryP): List[E] = {
      val (fw, df) = current.store
      new GListSyntax(fw.value).toList.map(df).collect {
        case Alive(tv) => tv.value
      }
    }

    def sequence(using QueryP): Long = {
      val (fw, _) = current.store
      fw.counter
    }

    private def findInsertIndex(state: RGA[E], n: Int): Option[Int] = state match {
      case WithContext((fw, df), _) =>
        fw.value.toLazyList.zip(LazyList.from(1)).filter {
          case (dot, _) => df(dot) match {
              case Alive(_) => true
              case Dead()   => false
            }
        }.map(_._2).prepended(0).lift(n)
    }

    def insert(i: Int, e: E)(using MutationIdP): C = {
      val (fw, df) = current.store
      val nextDot  = current.context.nextDot(replicaID)

      findInsertIndex(current, i) match {
        case None => deltaState[E].bottom
        case Some(glistInsertIndex) =>
          val glistDelta = fw.map(gl => GListSyntax(gl).insert(glistInsertIndex, nextDot)(using withID(replicaID)))
          val dfDelta    = DotFun[RGANode[E]].empty.store + (nextDot -> Alive(TimedVal(e, replicaID)))

          deltaState[E].make(
            epoche = glistDelta,
            df = dfDelta,
            cc = CausalContext.single(nextDot)
          )
      }
    }.mutator

    def insertAll(i: Int, elems: Iterable[E])(using MutationP, IdentifierP): C = {
      val (fw, df) = current.store
      val nextDot  = current.context.nextDot(replicaID)

      val nextDots = List.iterate(nextDot, elems.size) {
        case Dot(c, r) => Dot(c + 1, r)
      }

      findInsertIndex(current, i) match {
        case None => deltaState[E].bottom
        case Some(glistInsertIndex) =>
          val glistDelta = fw.map(gl => GListSyntax(gl).insertAll(glistInsertIndex, nextDots)(using summon, withID(replicaID)))
          val dfDelta    = DotFun[RGANode[E]].empty.store ++ (nextDots zip elems.map(e => Alive(TimedVal(e, replicaID))))

          deltaState[E].make(
            epoche = glistDelta,
            df = dfDelta,
            cc = CausalContext.fromSet(nextDots.toSet)
          )
      }
    }.mutator

    private def updateRGANode(state: RGA[E], i: Int, newNode: RGANode[E]): RGA[E] = {
      val (fw, df) = state.store
      fw.value.toLazyList.filter { dot =>
        df(dot) match {
          case Alive(_) => true
          case Dead()   => false
        }
      }.lift(i) match {
        case None => deltaState[E].bottom
        case Some(d) =>
          deltaState[E].make(df = DotFun[RGANode[E]].empty.store + (d -> newNode))
      }
    }

    def update(i: Int, e: E)(using MutationIdP): C =
      updateRGANode(current, i, Alive(TimedVal(e, replicaID))).mutator

    def delete(i: Int)(using MutationIdP): C = updateRGANode(current, i, Dead[E]()).mutator

    private def updateRGANodeBy(
        state: RGA[E],
        cond: E => Boolean,
        newNode: RGANode[E]
    ): RGA[E] =
      state match {
        case WithContext((_, df), _) =>
          val toUpdate = df.toList.collect {
            case (d, Alive(tv)) if cond(tv.value) => d
          }

          val dfDelta = DotFun[RGANode[E]].empty.store ++ toUpdate.map(_ -> newNode)

          deltaState[E].make(df = dfDelta)
      }

    def updateBy(cond: E => Boolean, e: E)(using MutationIdP): C =
      updateRGANodeBy(current, cond, Alive(TimedVal(e, replicaID))).mutator

    def deleteBy(cond: E => Boolean)(using MutationIdP): C =
      updateRGANodeBy(current, cond, Dead[E]()).mutator

    def purgeTombstones()(using MutationIdP): C = {
      val (epoche, df) = current.store
      val toRemove = df.collect {
        case (dot, Dead()) => dot
      }.toSet

      val golistPurged = epoche.value.without(toRemove)

      deltaState[E].make(
        epoche = epoche.epocheWrite(golistPurged),
        cc = CausalContext.fromSet(toRemove)
      ).mutator
    }

    def clear()(using MutationIdP): C = {
      val (_, WithContext(_, cc)) = current.store
      deltaState[E].make(
        cc = cc
      ).mutator
    }

    def prepend(e: E)(using MutationIdP): C = insert(0, e)

    def append(e: E)(using MutationIdP): C = insert(size, e)

    def prependAll(elems: Iterable[E])(using MutationIdP): C = insertAll(0, elems)

    def appendAll(elems: Iterable[E])(using MutationIdP): C = insertAll(size, elems)

  }
}
