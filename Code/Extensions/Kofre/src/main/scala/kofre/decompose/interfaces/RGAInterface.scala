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

case class RGA[E](order: Epoche[GListInterface.GList[Dot]], meta: Map[Dot, RGAInterface.RGANode[E]])
object RGAInterface {

  def empty[E]: RGA[E] = RGA(Epoche.empty, Map.empty)

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

  private class DeltaStateFactory[E] {
    given ContextDecompose[Epoche[GListInterface.GList[Dot]]] = ContextDecompose.UIJDLatticeAsDecomposableDotStore

    def make(
        epoche: Epoche[GListInterface.GList[Dot]] = empty._1,
        df: Map[Dot, RGANode[E]] = empty._2,
        cc: CausalContext = CausalContext.empty
    ): WithContext[RGA[E]] = WithContext(RGA(epoche, df), cc)
  }

  private def deltaState[E]: DeltaStateFactory[E] = new DeltaStateFactory[E]

  implicit class RGASyntax[C, E](container: C)(using ArdtOpsContains[C, RGA[E]])
      extends OpsSyntaxHelper[C, RGA[E]](container) {

    def read(i: Int)(using QueryP): Option[E] = {
      val RGA(fw, df) = current
      fw.value.toLazyList.map(df).collect {
        case Alive(tv) => tv.value
      }.lift(i)
    }

    def size(using QueryP): Int = {
      val RGA(_, df) = current
      df.values.count {
        case Dead()   => false
        case Alive(_) => true
      }
    }

    def toList(using QueryP): List[E] = {
      val RGA(fw, df) = current
      new GListSyntax(fw.value).toList.map(df).collect {
        case Alive(tv) => tv.value
      }
    }

    def sequence(using QueryP): Long = {
      val RGA(fw, _) = current
      fw.counter
    }

    private def findInsertIndex(state: RGA[E], n: Int): Option[Int] = state match {
      case RGA(fw, df) =>
        fw.value.toLazyList.zip(LazyList.from(1)).filter {
          case (dot, _) => df(dot) match {
              case Alive(_) => true
              case Dead()   => false
            }
        }.map(_._2).prepended(0).lift(n)
    }

    def insert(i: Int, e: E)(using CausalMutationP, IdentifierP): C = {
      val RGA(fw, df) = current
      val nextDot     = context.nextDot(replicaID)

      findInsertIndex(current, i) match {
        case None => WithContext(RGAInterface.empty[E])
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

    def insertAll(i: Int, elems: Iterable[E])(using CausalMutationP, IdentifierP): C = {
      val RGA(fw, df) = current
      val nextDot     = context.nextDot(replicaID)

      val nextDots = List.iterate(nextDot, elems.size) {
        case Dot(c, r) => Dot(c + 1, r)
      }

      findInsertIndex(current, i) match {
        case None => WithContext(RGAInterface.empty)
        case Some(glistInsertIndex) =>
          val glistDelta =
            fw.map(gl => GListSyntax(gl).insertAll(glistInsertIndex, nextDots)(using summon, withID(replicaID)))
          val dfDelta = DotFun[RGANode[E]].empty.store ++ (nextDots zip elems.map(e => Alive(TimedVal(e, replicaID))))

          deltaState[E].make(
            epoche = glistDelta,
            df = dfDelta,
            cc = CausalContext.fromSet(nextDots.toSet)
          )
      }
    }.mutator

    private def updateRGANode(state: RGA[E], i: Int, newNode: RGANode[E]): WithContext[RGA[E]] = {
      val RGA(fw, df) = state
      fw.value.toLazyList.filter { dot =>
        df(dot) match {
          case Alive(_) => true
          case Dead()   => false
        }
      }.lift(i) match {
        case None => WithContext(RGAInterface.empty)
        case Some(d) =>
          deltaState[E].make(df = DotFun[RGANode[E]].empty.store + (d -> newNode))
      }
    }

    def update(i: Int, e: E)(using CausalMutationP, IdentifierP): C =
      updateRGANode(current, i, Alive(TimedVal(e, replicaID))).mutator

    def delete(i: Int)(using CausalMutationP, IdentifierP): C = updateRGANode(current, i, Dead[E]()).mutator

    private def updateRGANodeBy(
        state: RGA[E],
        cond: E => Boolean,
        newNode: RGANode[E]
    ): WithContext[RGA[E]] = {
      val RGA(_, df) = state
      val toUpdate = df.toList.collect {
        case (d, Alive(tv)) if cond(tv.value) => d
      }

      val dfDelta = DotFun[RGANode[E]].empty.store ++ toUpdate.map(_ -> newNode)

      deltaState[E].make(df = dfDelta)
    }

    def updateBy(cond: E => Boolean, e: E)(using CausalMutationP, IdentifierP): C =
      updateRGANodeBy(current, cond, Alive(TimedVal(e, replicaID))).mutator

    def deleteBy(cond: E => Boolean)(using CausalMutationP, IdentifierP): C =
      updateRGANodeBy(current, cond, Dead[E]()).mutator

    def purgeTombstones()(using CausalMutationP, IdentifierP): C = {
      val RGA(epoche, df) = current
      val toRemove = df.collect {
        case (dot, Dead()) => dot
      }.toSet

      val golistPurged = epoche.value.without(toRemove)

      deltaState[E].make(
        epoche = epoche.epocheWrite(golistPurged),
        cc = CausalContext.fromSet(toRemove)
      ).mutator
    }

    def clear()(using CausalMutationP): C = {
      deltaState[E].make(
        cc = context
      ).mutator
    }

    def prepend(e: E)(using CausalMutationP, IdentifierP): C = insert(0, e)

    def append(e: E)(using CausalMutationP, IdentifierP): C = insert(size, e)

    def prependAll(elems: Iterable[E])(using CausalMutationP, IdentifierP): C = insertAll(0, elems)

    def appendAll(elems: Iterable[E])(using CausalMutationP, IdentifierP): C = insertAll(size, elems)

  }
}
