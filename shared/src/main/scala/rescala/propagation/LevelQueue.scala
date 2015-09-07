package rescala.propagation

import java.lang.{Boolean => jlBool}

import rescala.graph.{Spores, Reactive}
import rescala.propagation.LevelQueue.QueueElement
import rescala.turns.Turn

import scala.collection.SortedSet

class LevelQueue[S <: Spores]()(implicit val currenTurn: Turn[S]) {

  private var elements = SortedSet[QueueElement[S]]()

  def currentLevel(): Int = elements.headOption.fold(Int.MaxValue)(_.level)

  /** mark the reactive as needing a reevaluation */
  def enqueue(minLevel: Int, needsEvaluate: Boolean = true)(dep: Reactive[S]): Unit = {
    elements += QueueElement[S](dep.level.get, dep, minLevel, needsEvaluate)
  }

  def remove(reactive: Reactive[S]): Unit = {
    elements = elements.filter(qe => qe.reactive ne reactive)
  }

  final def handleHead(queueElement: QueueElement[S], evaluator: Reactive[S] => Unit): Unit = {
    val QueueElement(headLevel, head, headMinLevel, doEvaluate) = queueElement
    if (headLevel < headMinLevel) {
      head.level.set(headMinLevel)
      val reevaluate = if (doEvaluate) true
      else if (elements.isEmpty) false
      else if (elements.head.reactive ne head) false
      else {
        elements = elements.tail
        true
      }
      enqueue(headMinLevel, reevaluate)(head)
      head.outgoing.get.foreach { r =>
        if (r.level.get <= headMinLevel)
          enqueue(headMinLevel + 1, needsEvaluate = false)(r)
      }
    }
    else if (doEvaluate) {
      evaluator(head)
    }
  }

  /** Evaluates all the elements in the queue */
  def evaluateQueue(evaluator: Reactive[S] => Unit) = {
    while (elements.nonEmpty) {
      val head = elements.head
      elements = elements.tail
      handleHead(head, evaluator)
    }
  }

  def clear() = elements = SortedSet[QueueElement[S]]()

}

object LevelQueue {

  private case class QueueElement[S <: Spores](level: Int, reactive: Reactive[S], minLevel: Int, needsEvaluate: Boolean)
  private implicit val ordering: Ordering[QueueElement[_]] = new Ordering[QueueElement[_]] {
    override def compare(x: QueueElement[_], y: QueueElement[_]): Int = {
      val levelDiff = Integer.compare(x.level, y.level)
      if (levelDiff != 0) levelDiff
      else {
        val hashDiff = Integer.compare(x.reactive.hashCode, y.reactive.hashCode)
        if (hashDiff != 0) hashDiff
        else jlBool.compare(x.needsEvaluate, y.needsEvaluate)
      }
    }
  }
  private implicit def lqo[S <: Spores]: Ordering[QueueElement[S]] = ordering.asInstanceOf[Ordering[QueueElement[S]]]
}

