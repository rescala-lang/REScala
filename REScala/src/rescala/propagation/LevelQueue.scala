package rescala.propagation

import java.lang.{Boolean => jlBool}

import rescala.graph.Reactive
import rescala.turns.Turn

import scala.collection.SortedSet

class LevelQueue(evaluator: Reactive => Unit)(implicit val currenTurn: Turn) {

  private var evalQueue = SortedSet[QueueElement]()

  /** mark the reactive as needing a reevaluation */
  def enqueue(minLevel: Int, needsEvaluate: Boolean = true)(dep: Reactive): Unit = {
    evalQueue += QueueElement(dep.level.get, dep, minLevel, needsEvaluate)
  }

  final def evaluate(queueElement: QueueElement): Unit = {
    val QueueElement(headLevel, head, headMinLevel, doEvaluate) = queueElement
    if (headLevel < headMinLevel) {
      head.level.set(headMinLevel)
      val reevaluate = if (doEvaluate) true
      else if (evalQueue.isEmpty) false
      else if (evalQueue.head.reactive ne head) false
      else {
        evalQueue = evalQueue.tail
        true
      }
      enqueue(headMinLevel, reevaluate)(head)
      head.dependants.get.foreach(enqueue(headMinLevel + 1, reevaluate))
    }
    else if (doEvaluate) {
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

  private case class QueueElement(level: Int, reactive: Reactive, minLevel: Int, needsEvaluate: Boolean)
  private implicit def ordering: Ordering[QueueElement] = new Ordering[QueueElement] {
    override def compare(x: QueueElement, y: QueueElement): Int = {
      val levelDiff = Integer.compare(x.level, y.level)
      if (levelDiff != 0) levelDiff
      else {
        val hashDiff = Integer.compare(x.reactive.hashCode(), y.reactive.hashCode())
        if (hashDiff != 0) hashDiff
        else jlBool.compare(x.needsEvaluate, y.needsEvaluate)
      }
    }
  }
}

