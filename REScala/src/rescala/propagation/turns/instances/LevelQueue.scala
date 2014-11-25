package rescala.propagation.turns.instances

import rescala.propagation.Reactive
import rescala.propagation.turns.Turn

import scala.collection.mutable

class LevelQueue(evaluator: Reactive => Unit)(implicit val currenTurn: Turn) {

  protected val evalQueue = new mutable.PriorityQueue[QueueElement]()

  /** mark the reactive as needing a reevaluation */
  def enqueue(minLevel: Int)(dep: Reactive): Unit = {
    if (!evalQueue.exists { case QueueElement(_, reactive, _) => reactive eq dep }) {
      evalQueue += QueueElement(dep.level.get, dep, minLevel)
    }
  }

  def evaluate(queueElement: QueueElement): Unit = {
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

}

case class QueueElement(level: Int, reactive: Reactive, newLevel: Int)
object QueueElement {

  implicit val ordering: Ordering[QueueElement] = new Ordering[QueueElement] {
    override def compare(x: QueueElement, y: QueueElement): Int = y.level.compareTo(x.level)
  }
}