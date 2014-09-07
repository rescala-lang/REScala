package rescala

import rescala.log.ReactiveLogging
import scala.collection.mutable

/**
 * The engine that schedules the (glitch-free) evaluation
 * of the nodes in the dependency graph.
 */
object ReactiveEngine extends ReactiveLogging {

  private val evalQueue = new mutable.PriorityQueue[(Int, Dependent)]()(new Ordering[(Int, Dependent)] {
    override def compare(x: (Int, Dependent), y: (Int, Dependent)): Int = y._1.compareTo(x._1)
  })

  /** Adds a dependant to the eval queue, duplicates are allowed */
  def addToEvalQueue(dep: Dependent): Unit = {
      if (!evalQueue.exists { case (_, elem) => elem eq dep }) {
        log.nodeScheduled(dep)
        evalQueue.+=((dep.level, dep))
      }
  }

  /** Evaluates all the elements in the queue */
  def startEvaluation() = {
    while (evalQueue.nonEmpty) {
      val (level, head) = evalQueue.dequeue()
      // check the level if it changed queue again
      if (level == head.level) head.triggerReevaluation()
      else addToEvalQueue(head)
    }
  }
}
