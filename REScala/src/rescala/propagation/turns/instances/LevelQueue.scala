package rescala.propagation.turns.instances

import rescala.propagation.Reactive
import rescala.propagation.turns.Turn

import scala.collection.mutable

class LevelQueue(evaluator: Reactive => Unit)(implicit val currenTurn: Turn) {

  private val evalQueue = new mutable.PriorityQueue[QueueElement]()

  /** mark the reactive as needing a reevaluation */
  def enqueue(minLevel: Int)(dep: Reactive): Unit = {
    if (!evalQueue.exists { case elem@QueueElement(_, reactive, oldMinLevel) =>
      if (reactive eq dep) {
        elem.newLevel = math.max(oldMinLevel, minLevel)
        true
      }
      else false
    }) {
      evalQueue += QueueElement(dep.level.get, dep, minLevel)
    }
  }

  final def evaluate(queueElement: QueueElement): Unit = {
    val QueueElement(headLevel, head, headMinLevel) = queueElement
    if (headLevel < headMinLevel) {
      head.level.set(headMinLevel)
      enqueue(headMinLevel)(head)
      head.dependants.get.foreach(enqueue(headMinLevel + 1))
    }
    else {
      evaluator(head)
    }
  }

  /** Evaluates all the elements in the queue */
  def evaluateQueue() = {
    while (evalQueue.nonEmpty) {
      evaluate(evalQueue.dequeue())
    }
  }

  private case class QueueElement(level: Int, reactive: Reactive, var newLevel: Int)
  private implicit def ordering: Ordering[QueueElement] = new Ordering[QueueElement] {
    override def compare(x: QueueElement, y: QueueElement): Int = y.level.compareTo(x.level)
  }
}

