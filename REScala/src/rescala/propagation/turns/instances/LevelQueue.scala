package rescala.propagation.turns.instances

import rescala.propagation.EvaluationResult.{Dynamic, Static}
import rescala.propagation.Reactive
import rescala.propagation.turns.Turn
import rescala.propagation.turns.creation.TurnFactory

import scala.annotation.tailrec
import scala.collection.mutable

abstract class LevelQueue()(implicit val currenTurn: Turn) {

  protected val evalQueue = new mutable.PriorityQueue[(Int, Reactive)]()(TurnFactory.reactiveOrdering)


  /** mark the reactive as needing a reevaluation */
  def enqueue(dep: Reactive): Unit = {
    if (!evalQueue.exists { case (_, elem) => elem eq dep }) {
      evalQueue.+=((dep.level.get, dep))
    }
  }

  /** evaluates a single reactive */
  def evaluate(head: Reactive): Unit

  /** Evaluates all the elements in the queue */
  def evaluateQueue() = {
    while (evalQueue.nonEmpty) {
      val (level, head) = evalQueue.dequeue()
      // check the level if it changed queue again
      if (level != head.level.get) enqueue(head)
      else evaluate(head)
    }
  }

}
