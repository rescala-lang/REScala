package rescala.propagation.turns.instances

import rescala.propagation.Reactive
import rescala.propagation.turns.Turn

import scala.collection.SortedSet

class LevelQueue(evaluator: Reactive => Unit)(implicit val currenTurn: Turn) {

  private var evalQueue = SortedSet[QueueElement]()

  /** mark the reactive as needing a reevaluation */
  def enqueue(minLevel: Int)(dep: Reactive): Unit = {
    evalQueue += QueueElement(dep.level.get, dep, minLevel)
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
      val head = evalQueue.head
      evalQueue = evalQueue.tail
      evaluate(head)
    }
  }

  private case class QueueElement(level: Int, reactive: Reactive, minLevel: Int)
  private implicit def ordering: Ordering[QueueElement] = new Ordering[QueueElement] {
    override def compare(x: QueueElement, y: QueueElement): Int = {
      val levelDiff = x.level.compareTo(y.level)
      if (levelDiff != 0) levelDiff
      else x.reactive.hashCode().compareTo(y.reactive.hashCode())
    }
  }
}

