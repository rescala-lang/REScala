package kofre.datatypes

import kofre.base.{Bottom, Lattice}
import kofre.datatypes.Epoche
import kofre.datatypes.alternatives.lww.TimedVal
import kofre.dotted.{DotFun, Dotted, DottedLattice}
import kofre.syntax.{OpsSyntaxHelper, ReplicaId}
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

  given rgaContext[E]: DottedLattice[ReplicatedList[E]] = DottedLattice.derived[ReplicatedList[E]]

  given bottom[E]: Bottom[ReplicatedList[E]] = new:
    override def empty: ReplicatedList[E] = ReplicatedList.empty

  enum Node[A]:
    case Alive[A](v: TimedVal[A]) extends Node[A]
    case Dead[A]()                extends Node[A]
  import Node.{Alive, Dead}

  object Node {
    implicit def RGANodeAsUIJDLattice[A]: Lattice[Node[A]] = new Lattice[Node[A]] {
      override def lteq(left: Node[A], right: Node[A]): Boolean = (left, right) match {
        case (Dead(), _)            => false
        case (_, Dead())            => true
        case (Alive(lv), Alive(rv)) => rv.timestamp > lv.timestamp
      }

      /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
      override def decompose(state: Node[A]): Iterable[Node[A]] = List(state)

      /** By assumption: associative, commutative, idempotent. */
      override def merge(left: Node[A], right: Node[A]): Node[A] = (left, right) match {
        case (Alive(lv), Alive(rv)) => Alive(Lattice[TimedVal[A]].merge(lv, rv))
        case _                      => Dead()
      }
    }
  }

  private class DeltaStateFactory[E] {
    given DottedLattice[Epoche[GrowOnlyList[Dot]]] = DottedLattice.liftLattice

    def make(
        epoche: Epoche[GrowOnlyList[Dot]] = empty._1,
        df: DotFun[Node[E]] = DotFun.empty,
        cc: Dots = Dots.empty
    ): Dotted[ReplicatedList[E]] = Dotted(ReplicatedList(epoche, df), cc)
  }

  private def deltaState[E]: DeltaStateFactory[E] = new DeltaStateFactory[E]

  extension [C, E](container: C)
    def multiVersionRegister: syntax[C, E] = syntax(container)

  implicit class syntax[C, E](container: C)
      extends OpsSyntaxHelper[C, ReplicatedList[E]](container) {

    def read(using PermQuery)(i: Int): Option[E] = {
      val ReplicatedList(fw, df) = current
      fw.value.toLazyList.map(df.repr).collect {
        case Alive(tv) => tv.payload
      }.lift(i)
    }

    def size(using PermQuery): Int = {
      val ReplicatedList(_, df) = current
      df.repr.values.count {
        case Dead()   => false
        case Alive(_) => true
      }
    }

    def toList(using PermQuery): List[E] = {
      val ReplicatedList(fw, df) = current
      fw.value.growOnlyList.toList.map(df.repr).collect {
        case Alive(tv) => tv.payload
      }
    }

    def sequence(using PermQuery): Long = {
      val ReplicatedList(fw, _) = current
      fw.counter
    }

    private def findInsertIndex(state: ReplicatedList[E], n: Int): Option[Int] = state match {
      case ReplicatedList(fw, df) =>
        fw.value.toLazyList.zip(LazyList.from(1)).filter {
          case (dot, _) => df.repr(dot) match {
              case Alive(_) => true
              case Dead()   => false
            }
        }.map(_._2).prepended(0).lift(n)
    }

    def insert(using ReplicaId, PermCausalMutate)(i: Int, e: E): C = {
      val ReplicatedList(fw, df) = current
      val nextDot                = context.nextDot(replicaId)

      findInsertIndex(current, i) match {
        case None => Dotted(ReplicatedList.empty[E])
        case Some(glistInsertIndex) =>
          val glistDelta = fw.map { gl =>
            gl.insertGL(glistInsertIndex, nextDot)
          }
          val dfDelta = DotFun.single(nextDot, Alive(TimedVal.now(e, replicaId)))

          deltaState[E].make(
            epoche = glistDelta,
            df = dfDelta,
            cc = Dots.single(nextDot)
          )
      }
    }.mutator

    def insertAll(using ReplicaId, PermCausalMutate)(i: Int, elems: Iterable[E]): C = {
      val ReplicatedList(fw, df) = current
      val nextDot                = context.nextDot(replicaId)

      val nextDots = List.iterate(nextDot, elems.size) {
        case Dot(c, r) => Dot(c, r + 1)
      }

      findInsertIndex(current, i) match {
        case None => Dotted(ReplicatedList.empty)
        case Some(glistInsertIndex) =>
          val glistDelta =
            fw.map { gl =>
              gl.insertAllGL(glistInsertIndex, nextDots)
            }
          val dfDelta = DotFun.empty[Node[E]].repr ++ (nextDots zip elems.map(e => Alive(TimedVal.now(e, replicaId))))

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
        df.repr(dot) match {
          case Alive(_) => true
          case Dead()   => false
        }
      }.lift(i) match {
        case None => Dotted(ReplicatedList.empty)
        case Some(d) =>
          deltaState[E].make(df = DotFun.single(d, newNode))
      }
    }

    def update(using ReplicaId, PermCausalMutate)(i: Int, e: E): C =
      updateRGANode(current, i, Alive(TimedVal.now(e, replicaId))).mutator

    def delete(using ReplicaId, PermCausalMutate)(i: Int): C = updateRGANode(current, i, Dead[E]()).mutator

    private def updateRGANodeBy(
        state: ReplicatedList[E],
        cond: E => Boolean,
        newNode: Node[E]
    ): Dotted[ReplicatedList[E]] = {
      val ReplicatedList(_, df) = state
      val toUpdate = df.repr.toList.collect {
        case (d, Alive(tv)) if cond(tv.payload) => d
      }

      deltaState[E].make(df = DotFun(toUpdate.iterator.map(_ -> newNode).toMap))
    }

    def updateBy(using ReplicaId, PermCausalMutate)(cond: E => Boolean, e: E): C =
      updateRGANodeBy(current, cond, Alive(TimedVal.now(e, replicaId))).mutator

    def deleteBy(using ReplicaId, PermCausalMutate)(cond: E => Boolean): C =
      updateRGANodeBy(current, cond, Dead[E]()).mutator

    def purgeTombstones(using ReplicaId, PermCausalMutate)(): C = {
      val ReplicatedList(epoche, df) = current
      val toRemove = df.repr.collect {
        case (dot, Dead()) => dot
      }.toSet

      val golistPurged = epoche.value.without(toRemove)

      deltaState[E].make(
        epoche = epoche.epocheWrite(golistPurged),
        cc = Dots.from(toRemove)
      ).mutator
    }

    def clear(using PermCausalMutate)(): C = {
      deltaState[E].make(
        cc = context
      ).mutator
    }

    def prepend(using ReplicaId, PermCausalMutate)(e: E): C = insert(0, e)

    def append(using ReplicaId, PermCausalMutate)(e: E): C = insert(size, e)

    def prependAll(using ReplicaId, PermCausalMutate)(elems: Iterable[E]): C = insertAll(0, elems)

    def appendAll(using ReplicaId, PermCausalMutate)(elems: Iterable[E]): C = insertAll(size, elems)

  }
}
