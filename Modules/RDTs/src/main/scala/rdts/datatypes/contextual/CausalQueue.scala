package rdts.datatypes.contextual

import rdts.base.{Bottom, Lattice}
import rdts.datatypes.contextual.CausalQueue.QueueElement
import rdts.dotted.{Dotted, HasDots}
import rdts.syntax.{LocalUid, OpsSyntaxHelper}
import rdts.time.{Dot, Dots, VectorClock}

import scala.collection.immutable.Queue

case class CausalQueue[T](values: Queue[QueueElement[T]]) {

  type Delta = Dotted[CausalQueue[T]]

  def enqueue(using LocalUid)(e: T)(using context: Dots): Delta =
    val time = context.clock.inc(LocalUid.replicaId)
    val dot  = time.dotOf(LocalUid.replicaId)
    Dotted(CausalQueue(Queue(QueueElement(e, dot, time))), Dots.single(dot))

  def head =
    val QueueElement(e, _, _) = values.head
    e

  def dequeue(): Delta =
    val QueueElement(_, dot, _) = values.head
    Dotted(CausalQueue.empty, Dots.single(dot))

  def removeBy(p: T => Boolean): Delta =
    val toRemove = values.filter(e => p(e.value)).map(_.dot)
    Dotted(CausalQueue.empty, Dots.from(toRemove))

  def elements: Queue[T] =
    values.map(_.value)

}

object CausalQueue:
  case class QueueElement[+T](value: T, dot: Dot, order: VectorClock)

  given elementLattice[T]: Lattice[QueueElement[T]] with {
    override def merge(left: QueueElement[T], right: QueueElement[T]): QueueElement[T] =
      if left.order < right.order then right else left
  }

  def empty[T]: CausalQueue[T] = CausalQueue(Queue())

  given hasDots[A]: HasDots[CausalQueue[A]] with {
    extension (value: CausalQueue[A])
      override def dots: Dots = Dots.from(value.values.view.map(_.dot))

      override def removeDots(dots: Dots): Option[CausalQueue[A]] =
        Some(CausalQueue(value.values.filter(qe => !dots.contains(qe.dot))))
  }

  given bottomInstance[T]: Bottom[CausalQueue[T]] = Bottom.derived

  given lattice[A]: Lattice[CausalQueue[A]] with {
    override def merge(left: CausalQueue[A], right: CausalQueue[A]): CausalQueue[A] =
      CausalQueue:
        (left.values concat right.values)
          .sortBy { qe => qe.order }(using VectorClock.vectorClockTotalOrdering).distinct

  }
