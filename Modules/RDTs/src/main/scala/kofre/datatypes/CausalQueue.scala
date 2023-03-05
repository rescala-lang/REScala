package kofre.datatypes

import kofre.base.Lattice.Operators
import kofre.base.{Bottom, Uid, Lattice}
import kofre.datatypes.CausalQueue.QueueElement
import kofre.dotted.{Dotted, DottedLattice, HasDots}
import kofre.syntax.{OpsSyntaxHelper, ReplicaId}
import kofre.time.{Dot, Dots, VectorClock}

import scala.collection.immutable.Queue

case class CausalQueue[+T](values: Queue[QueueElement[T]])

object CausalQueue:
  case class QueueElement[+T](value: T, dot: Dot, order: VectorClock)

  given elementLattice[T]: Lattice[QueueElement[T]] with {
    override def merge(left: QueueElement[T], right: QueueElement[T]): QueueElement[T] =
      if left.order < right.order then right else left
  }

  def empty[T]: CausalQueue[T] = CausalQueue(Queue())

  given hasDots: HasDots[CausalQueue[Any]] with {
    override def getDots(a: CausalQueue[Any]): Dots = Dots.from(a.values.view.map(_.dot))
  }

  given bottomInstance[T]: Bottom[CausalQueue[T]] = Bottom.derived

  extension [C, T](container: C)
    def causalQueue: syntax[C, T] = syntax(container)

  implicit class syntax[C, T](container: C)
      extends OpsSyntaxHelper[C, CausalQueue[T]](container) {

    def enqueue(using ReplicaId, PermCausalMutate)(e: T): C =
      val time = context.clock.inc(replicaId)
      val dot  = time.dotOf(replicaId)
      Dotted(CausalQueue(Queue(QueueElement(e, dot, time))), Dots.single(dot)).mutator

    def head(using PermQuery) =
      val QueueElement(e, _, _) = current.values.head
      e

    def dequeue(using PermCausalMutate)(): C =
      val QueueElement(_, dot, _) = current.values.head
      Dotted(CausalQueue.empty, Dots.single(dot)).mutator

    def removeBy(using PermCausalMutate)(p: T => Boolean) =
      val toRemove = current.values.filter(e => p(e.value)).map(_.dot)
      Dotted(CausalQueue.empty, Dots.from(toRemove)).mutator

    def elements(using PermQuery): Queue[T] =
      current.values.map(_.value)

  }

  given lattice[A]: DottedLattice[CausalQueue[A]] with {
    override def mergePartial(left: Dotted[CausalQueue[A]], right: Dotted[CausalQueue[A]]): CausalQueue[A] =

      val leftDots  = Dots.from(left.store.values.map(_.dot))
      val rightDots = Dots.from(right.store.values.map(_.dot))

      val li = left.store.values.iterator.filter(qe => !(right.context subtract rightDots).contains(qe.dot))
      val ri = right.store.values.iterator.filter(qe => !(left.context subtract leftDots).contains(qe.dot))

      val res = (li concat ri).to(Queue)
        .sortBy { qe => qe.order }(using VectorClock.vectorClockTotalOrdering).distinct
      CausalQueue(res)
  }
