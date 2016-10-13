package rescala.propagation

import java.lang.{Boolean => jlBool}

import rescala.graph.{LevelStruct, Reactive, Struct}
import rescala.propagation.LevelQueue.QueueElement

import scala.collection.immutable.SortedSet

/**
  * Level-based queue used the determine an evaluation order for reactive elements
  *
  * @param currentTurn Turn of the evaluation
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
private[propagation] class LevelQueue[S <: LevelStruct]()(implicit val currentTurn: Turn[S]) {

  private var elements = SortedSet.empty[QueueElement[S]]

  /**
    * Gets the level of the current head element of the queue (if existing).
    *
    * @return Level of the current queue head
    */
  def currentLevel(): Int = elements.headOption.fold(Int.MaxValue)(_.level)

  /**
    * Adds a new reactive element to the queue
    *
    * @param minLevel Minimum level to assign the the element (overrides the elements original level if larger)
    * @param needsEvaluate Indicates if the element needs re-evaulation itself
    * @param dep Element to add to the queue
    */
  def enqueue(minLevel: Int, needsEvaluate: Boolean = true)(dep: Reactive[S]): Unit = {
    elements += QueueElement[S](dep.bud.level, dep, minLevel, needsEvaluate)
  }

  /**
    * Removes a reactive element from the queue
    *
    * @param reactive Element to remove from the queue
    */
  def remove(reactive: Reactive[S]): Unit = {
    elements = elements.filter(qe => qe.reactive ne reactive)
  }

  /**
    * Handles a queue element by applying the given evaluator to it and scheduling the next elements for evaluation
    *
    * @param queueElement Element to evaluate
    * @param evaluator Evaluator function to apply to the element
    */
  final private def handleElement(queueElement: QueueElement[S], evaluator: Reactive[S] => Unit): Unit = {
    val QueueElement(headLevel, head, headMinLevel, doEvaluate) = queueElement
    if (headLevel < headMinLevel) {
      head.bud.updateLevel(headMinLevel)
      val reevaluate = if (doEvaluate) true
      else if (elements.isEmpty) false
      else if (elements.head.reactive ne head) false
      else {
        elements = elements.tail
        true
      }
      enqueue(headMinLevel, reevaluate)(head)
      head.bud.outgoing.foreach { r =>
        if (r.bud.level <= headMinLevel)
          enqueue(headMinLevel + 1, needsEvaluate = false)(r)
      }
    }
    else if (doEvaluate) {
      evaluator(head)
    }
  }

  /**
    * Evaluates all currently queued elements by applying the given evaluator to them.
    *
    * @param evaluator Evaluator function to apply to all elements
    */
  def evaluateQueue(evaluator: Reactive[S] => Unit) = {
    while (elements.nonEmpty) {
      val head = elements.head
      elements = elements.tail
      handleElement(head, evaluator)
    }
  }

  /**
    * Resets the queue by removing all enqueued elements
    */
  def clear() = elements = SortedSet[QueueElement[S]]()

}


private object LevelQueue {

  private case class QueueElement[S <: Struct](level: Int, reactive: Reactive[S], minLevel: Int, needsEvaluate: Boolean)
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
  private implicit def lqo[S <: Struct]: Ordering[QueueElement[S]] = ordering.asInstanceOf[Ordering[QueueElement[S]]]
}

