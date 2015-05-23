package rescala.propagation

import java.lang.{Boolean => jlBool}

import rescala.graph.Reactive
import rescala.propagation.LevelQueue.QueueElement
import rescala.turns.Turn

import scala.collection.SortedSet

class LevelQueue()(implicit val currentTurn: Turn) {

  private var elements = SortedSet[QueueElement]()

  /** mark the reactive as needing a reevaluation */
  def enqueue(minLevel: Int, needsEvaluate: Boolean = true)(dep: Reactive): Unit = {
    elements += QueueElement(dep.level.get, dep, minLevel, needsEvaluate)
  }

  def remove(reactive: Reactive): Unit = {
    elements = elements.filter(qe => qe.reactive ne reactive) // THat is wrong
  }

  final def handleHead(queueElement: QueueElement, evaluator: Reactive => Unit): Unit = {
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
  def evaluateQueue(evaluator: Reactive => Unit) = {
    while (elements.nonEmpty) {
      val head = elements.head
      elements = elements.tail
      handleHead(head, evaluator)
    }
  }

  def clear() = elements = SortedSet[QueueElement]()

}

object LevelQueue {

  private case class QueueElement(level: Int, reactive: Reactive, minLevel: Int, needsEvaluate: Boolean)
  private implicit val ordering: Ordering[QueueElement] = new Ordering[QueueElement] {
    override def compare(x: QueueElement, y: QueueElement): Int = {
      val levelDiff = Integer.compare(x.level, y.level)
      if (levelDiff != 0) levelDiff
      else {
        val hashDiff = Integer.compare(x.reactive.hashCode, y.reactive.hashCode)
        if (hashDiff != 0) hashDiff
        else jlBool.compare(x.needsEvaluate, y.needsEvaluate)
      }
    }
  }
}

