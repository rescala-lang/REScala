package kofre.datatypes

import kofre.base.{Bottom, DecomposeLattice}
import kofre.datatypes.{Epoche, TimedVal}
import kofre.decompose.*
import GrowOnlyList.{GListAsUIJDLattice, GListSyntax}
import kofre.dotted.{DotFun, Dotted, DottedDecompose, DottedLattice}
import kofre.syntax.PermIdMutate.withID
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermIdMutate, PermMutate}
import kofre.time.{Dot, Dots}

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

case class RGA[E](order: Epoche[GrowOnlyList[Dot]], meta: DotFun[RGA.RGANode[E]])
object RGA {

  def empty[E]: RGA[E] = kofre.datatypes.RGA(Epoche.empty, DotFun.empty)

  given rgaContext[E]: DottedDecompose[RGA[E]] = DottedDecompose.derived[RGA[E]]

  given bottom[E]: Bottom[RGA[E]] = new:
    override def empty: RGA[E] = RGA.empty

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

      /** By assumption: associative, commutative, idempotent. */
      override def merge(left: RGANode[A], right: RGANode[A]): RGANode[A] = (left, right) match {
        case (Alive(lv), Alive(rv)) => Alive(DecomposeLattice[TimedVal[A]].merge(lv, rv))
        case _                      => Dead()
      }
    }
  }

  private class DeltaStateFactory[E] {
    given DottedDecompose[Epoche[GrowOnlyList[Dot]]] = DottedDecompose.liftDecomposeLattice

    def make(
        epoche: Epoche[GrowOnlyList[Dot]] = empty._1,
        df: DotFun[RGANode[E]] = DotFun.empty,
        cc: Dots = Dots.empty
    ): Dotted[RGA[E]] = Dotted(RGA(epoche, df), cc)
  }

  private def deltaState[E]: DeltaStateFactory[E] = new DeltaStateFactory[E]

  implicit class RGAOps[C, E](container: C)(using ArdtOpsContains[C, RGA[E]])
      extends OpsSyntaxHelper[C, RGA[E]](container) {

    def read(i: Int)(using QueryP): Option[E] = {
      val RGA(fw, df) = current
      fw.value.toLazyList.map(df.store).collect {
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
      new GListSyntax(fw.value).toList.map(df.store).collect {
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
        case None => Dotted(RGA.empty[E])
        case Some(glistInsertIndex) =>
          val glistDelta = fw.map(gl => GListSyntax(gl).insert(glistInsertIndex, nextDot)(using withID(replicaID)))
          val dfDelta    = DotFun.empty[RGANode[E]] + (nextDot -> Alive(TimedVal(e, replicaID)))

          deltaState[E].make(
            epoche = glistDelta,
            df = dfDelta,
            cc = Dots.single(nextDot)
          )
      }
    }.mutator

    def insertAll(i: Int, elems: Iterable[E])(using CausalMutationP, IdentifierP): C = {
      val RGA(fw, df) = current
      val nextDot     = context.nextDot(replicaID)

      val nextDots = List.iterate(nextDot, elems.size) {
        case Dot(c, r) => Dot(c, r + 1)
      }

      findInsertIndex(current, i) match {
        case None => Dotted(RGA.empty)
        case Some(glistInsertIndex) =>
          val glistDelta =
            fw.map(gl => GListSyntax(gl).insertAll(glistInsertIndex, nextDots)(using summon, withID(replicaID)))
          val dfDelta = DotFun.empty[RGANode[E]] ++ (nextDots zip elems.map(e => Alive(TimedVal(e, replicaID))))

          deltaState[E].make(
            epoche = glistDelta,
            df = DotFun(dfDelta),
            cc = Dots.from(nextDots.toSet)
          )
      }
    }.mutator

    private def updateRGANode(state: RGA[E], i: Int, newNode: RGANode[E]): Dotted[RGA[E]] = {
      val RGA(fw, df) = state
      fw.value.toLazyList.filter { dot =>
        df(dot) match {
          case Alive(_) => true
          case Dead()   => false
        }
      }.lift(i) match {
        case None => Dotted(RGA.empty)
        case Some(d) =>
          deltaState[E].make(df = DotFun.empty[RGANode[E]] + (d -> newNode))
      }
    }

    def update(i: Int, e: E)(using CausalMutationP, IdentifierP): C =
      updateRGANode(current, i, Alive(TimedVal(e, replicaID))).mutator

    def delete(i: Int)(using CausalMutationP, IdentifierP): C = updateRGANode(current, i, Dead[E]()).mutator

    private def updateRGANodeBy(
        state: RGA[E],
        cond: E => Boolean,
        newNode: RGANode[E]
    ): Dotted[RGA[E]] = {
      val RGA(_, df) = state
      val toUpdate = df.toList.collect {
        case (d, Alive(tv)) if cond(tv.value) => d
      }

      val dfDelta = DotFun.empty[RGANode[E]] ++ toUpdate.map(_ -> newNode)

      deltaState[E].make(df = DotFun(dfDelta))
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
        cc = Dots.from(toRemove)
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
