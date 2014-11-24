package rescala.propagation.turns.instances

import rescala.propagation.EvaluationResult.{Dynamic, Static}
import rescala.propagation.Reactive
import rescala.propagation.turns.Turn
import rescala.propagation.turns.creation.TurnFactory

import scala.annotation.tailrec
import scala.collection.mutable

abstract class LevelQueue()(implicit val currenTurn: Turn) {

  protected val evalQueue = new mutable.PriorityQueue[QueueElement]()


  /** mark the reactive as needing a reevaluation */
  def enqueue(minLevel: Int)(dep: Reactive): Unit = {
    if (!evalQueue.exists { case QueueElement(_, reactive, _) => reactive eq dep }) {
      evalQueue.+=(QueueElement(dep.level.get, dep, minLevel))
    }
  }

  /** evaluates a single reactive */
  def evaluate(head: QueueElement): Unit

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