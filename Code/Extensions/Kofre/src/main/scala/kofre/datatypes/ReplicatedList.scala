package kofre.datatypes

import kofre.base.{Bottom, DecomposeLattice}
import kofre.datatypes.GrowOnlyList.{GListAsUIJDLattice, syntax}
import kofre.datatypes.{Epoche, TimedVal}
import kofre.dotted.{DotFun, Dotted, DottedDecompose, DottedLattice}
import kofre.syntax.PermIdMutate.withID
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermIdMutate, PermMutate}
import kofre.time.{Dot, Dots}

import scala.math.Ordering.Implicits.infixOrderingOps

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

case class ReplicatedList[E](order: Epoche[GrowOnlyList[Dot]], meta: DotFun[ReplicatedList.Node[E]])
object ReplicatedList {

  def empty[E]: ReplicatedList[E] = kofre.datatypes.ReplicatedList(Epoche.empty, DotFun.empty)

  given rgaContext[E]: DottedDecompose[ReplicatedList[E]] = DottedDecompose.derived[ReplicatedList[E]]

  given bottom[E]: Bottom[ReplicatedList[E]] = new:
    override def empty: ReplicatedList[E] = ReplicatedList.empty

  enum Node[A]:
    case Alive[A](v: TimedVal[A]) extends Node[A]
    case Dead[A]()                extends Node[A]
  import Node.{Alive, Dead}

  object Node {
    implicit def RGANodeAsUIJDLattice[A]: DecomposeLattice[Node[A]] = new DecomposeLattice[Node[A]] {
      override def lteq(left: Node[A], right: Node[A]): Boolean = (left, right) match {
        case (Dead(), _)            => false
        case (_, Dead())            => true
        case (Alive(lv), Alive(rv)) => rv > lv
      }

      /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
      override def decompose(state: Node[A]): Iterable[Node[A]] = List(state)

      /** By assumption: associative, commutative, idempotent. */
      override def merge(left: Node[A], right: Node[A]): Node[A] = (left, right) match {
        case (Alive(lv), Alive(rv)) => Alive(DecomposeLattice[TimedVal[A]].merge(lv, rv))
        case _                      => Dead()
      }
    }
  }

  private class DeltaStateFactory[E] {
    given DottedDecompose[Epoche[GrowOnlyList[Dot]]] = DottedDecompose.liftDecomposeLattice

    def make(
        epoche: Epoche[GrowOnlyList[Dot]] = empty._1,
        df: DotFun[Node[E]] = DotFun.empty,
        cc: Dots = Dots.empty
    ): Dotted[ReplicatedList[E]] = Dotted(ReplicatedList(epoche, df), cc)
  }

  private def deltaState[E]: DeltaStateFactory[E] = new DeltaStateFactory[E]

  implicit class RGAOps[C, E](container: C)(using ArdtOpsContains[C, ReplicatedList[E]])
      extends OpsSyntaxHelper[C, ReplicatedList[E]](container) {

    def read(i: Int)(using QueryP): Option[E] = {
      val ReplicatedList(fw, df) = current
      fw.value.toLazyList.map(df.store).collect {
        case Alive(tv) => tv.value
      }.lift(i)
    }

    def size(using QueryP): Int = {
      val ReplicatedList(_, df) = current
      df.values.count {
        case Dead()   => false
        case Alive(_) => true
      }
    }

    def toList(using QueryP): List[E] = {
      val ReplicatedList(fw, df) = current
      new syntax(fw.value).toList.map(df.store).collect {
        case Alive(tv) => tv.value
      }
    }

    def sequence(using QueryP): Long = {
      val ReplicatedList(fw, _) = current
      fw.counter
    }

    private def findInsertIndex(state: ReplicatedList[E], n: Int): Option[Int] = state match {
      case ReplicatedList(fw, df) =>
        fw.value.toLazyList.zip(LazyList.from(1)).filter {
          case (dot, _) => df(dot) match {
              case Alive(_) => true
              case Dead()   => false
            }
        }.map(_._2).prepended(0).lift(n)
    }

    def insert(i: Int, e: E)(using CausalMutationP, IdentifierP): C = {
      val ReplicatedList(fw, df) = current
      val nextDot                = context.nextDot(replicaID)

      findInsertIndex(current, i) match {
        case None => Dotted(ReplicatedList.empty[E])
        case Some(glistInsertIndex) =>
          val glistDelta = fw.map(gl => syntax(gl).insert(glistInsertIndex, nextDot)(using withID(replicaID)))
          val dfDelta    = DotFun.empty[Node[E]] + (nextDot -> Alive(TimedVal(e, replicaID)))

          deltaState[E].make(
            epoche = glistDelta,
            df = dfDelta,
            cc = Dots.single(nextDot)
          )
      }
    }.mutator

    def insertAll(i: Int, elems: Iterable[E])(using CausalMutationP, IdentifierP): C = {
      val ReplicatedList(fw, df) = current
      val nextDot                = context.nextDot(replicaID)

      val nextDots = List.iterate(nextDot, elems.size) {
        case Dot(c, r) => Dot(c, r + 1)
      }

      findInsertIndex(current, i) match {
        case None => Dotted(ReplicatedList.empty)
        case Some(glistInsertIndex) =>
          val glistDelta =
            fw.map(gl => syntax(gl).insertAll(glistInsertIndex, nextDots)(using summon, withID(replicaID)))
          val dfDelta = DotFun.empty[Node[E]] ++ (nextDots zip elems.map(e => Alive(TimedVal(e, replicaID))))

          deltaState[E].make(
            epoche = glistDelta,
            df = DotFun(dfDelta),
            cc = Dots.from(nextDots.toSet)
          )
      }
    }.mutator

    private def updateRGANode(state: ReplicatedList[E], i: Int, newNode: Node[E]): Dotted[ReplicatedList[E]] = {
      val ReplicatedList(fw, df) = state
      fw.value.toLazyList.filter { dot =>
        df(dot) match {
          case Alive(_) => true
          case Dead()   => false
        }
      }.lift(i) match {
        case None => Dotted(ReplicatedList.empty)
        case Some(d) =>
          deltaState[E].make(df = DotFun.empty[Node[E]] + (d -> newNode))
      }
    }

    def update(i: Int, e: E)(using CausalMutationP, IdentifierP): C =
      updateRGANode(current, i, Alive(TimedVal(e, replicaID))).mutator

    def delete(i: Int)(using CausalMutationP, IdentifierP): C = updateRGANode(current, i, Dead[E]()).mutator

    private def updateRGANodeBy(
        state: ReplicatedList[E],
        cond: E => Boolean,
        newNode: Node[E]
    ): Dotted[ReplicatedList[E]] = {
      val ReplicatedList(_, df) = state
      val toUpdate = df.toList.collect {
        case (d, Alive(tv)) if cond(tv.value) => d
      }

      val dfDelta = DotFun.empty[Node[E]] ++ toUpdate.map(_ -> newNode)

      deltaState[E].make(df = DotFun(dfDelta))
    }

    def updateBy(cond: E => Boolean, e: E)(using CausalMutationP, IdentifierP): C =
      updateRGANodeBy(current, cond, Alive(TimedVal(e, replicaID))).mutator

    def deleteBy(cond: E => Boolean)(using CausalMutationP, IdentifierP): C =
      updateRGANodeBy(current, cond, Dead[E]()).mutator

    def purgeTombstones()(using CausalMutationP, IdentifierP): C = {
      val ReplicatedList(epoche, df) = current
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
