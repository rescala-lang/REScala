package kofre.datatypes

import kofre.base.Lattice.Operators
import kofre.base.{Id, Lattice}
import kofre.datatypes.CausalQueue.QueueElement
import kofre.dotted.{Dotted, DottedLattice}
import kofre.syntax.OpsSyntaxHelper
import kofre.time.{Dot, Dots, VectorClock}

import scala.collection.immutable.Queue

case class CausalQueue[T](values: Queue[QueueElement[T]])

object CausalQueue:
  case class QueueElement[T](value: T, dot: Dot, order: VectorClock)

  def empty[T]: CausalQueue[T] = CausalQueue(Queue())

  extension [C, T](container: C)
    def causalQueue: syntax[C, T] = syntax(container)

  implicit class syntax[C, T](container: C)
      extends OpsSyntaxHelper[C, CausalQueue[T]](container) {

    def enqueue(using PermCausalMutate, PermId)(e: T): C =
      val time = context.clock.inc(replicaId)
      val dot  = time.dotOf(replicaId)
      Dotted(CausalQueue(Queue(QueueElement(e, dot, time))), Dots.single(dot)).mutator

    def head(using PermQuery) =
      val QueueElement(e, _, _) = current.values.head
      e

    def dequeue(using PermCausalMutate, PermId)(): C =
      val QueueElement(_, dot, _) = current.values.head
      Dotted(CausalQueue.empty, Dots.single(dot)).mutator

  }

  given lattice[A]: DottedLattice[CausalQueue[A]] with {
    override def mergePartial(left: Dotted[CausalQueue[A]], right: Dotted[CausalQueue[A]]): CausalQueue[A] =

      val li = left.store.values.iterator.filter(qe => !right.context.contains(qe.dot))
      val ri = right.store.values.iterator.filter(qe => !left.context.contains(qe.dot))

      val res = (li concat ri).to(Queue)
        .sortBy { qe => qe.order }(using VectorClock.vectorClockTotalOrdering)
      CausalQueue(res)
  }
