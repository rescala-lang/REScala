package rescala.levelbased

import java.util.PriorityQueue

import rescala.core.{Reactive, Struct}
import rescala.levelbased.LevelQueue.QueueElement

/**
  * Level-based queue used the determine an evaluation order for reactive elements
  *
  * @param currentTurn Turn of the evaluation
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
final private[levelbased] class LevelQueue[S <: LevelStruct](evaluator: LevelQueue.Evaluator[S])(currentTurn: LevelBasedPropagation[S]) {

  private val elements = new PriorityQueue[QueueElement[S]]()

  /**
    * Gets the level of the current head element of the queue (if existing).
    * Used to determine if newly created reactives have to be evaluated.
    *
    * @return Level of the current queue head
    */
  def currentLevel(): Int =
    if (elements.peek() == null) Int.MaxValue
    else elements.peek().level

  /**
    * Adds a new reactive element to the queue
    *
    * @param minLevel      Minimum level to assign the the element (overrides the elements original level if larger)
    * @param needsEvaluate Indicates if the element needs re-evaluation itself, otherwise it is just a level change
    * @param dep           Element to add to the queue
    */
  def enqueue(minLevel: Int, needsEvaluate: Boolean = true)(dep: Reactive[S]): Unit = {
    elements.offer(QueueElement[S](dep.state.level(), dep, minLevel, needsEvaluate))
  }

  /**
    * Handles a queue element by applying the given evaluator to it and scheduling the next elements for evaluation
    *
    * @param queueElement Element to evaluate
    */
  private def handleElement(queueElement: QueueElement[S]): Unit = {
    val QueueElement(headLevel, head, headMinLevel, reevaluate) = queueElement
    // handle level increases
    if (headLevel < headMinLevel) {
      head.state.updateLevel(headMinLevel)
      enqueue(headMinLevel, reevaluate)(head)
      head.state.outgoing().foreach { r =>
        if (r.state.level() <= headMinLevel)
          enqueue(headMinLevel + 1, needsEvaluate = false)(r)
      }
    }
    else if (reevaluate) {
      evaluator.evaluate(head)
    }
  }

  /**
    * Evaluates all currently queued elements by applying the given evaluator to them.
    */
  def evaluateQueue(): Unit = {
    var current = elements.poll()
    var next = elements.peek()
    while (current != null) {
      // if the current and next reactive are equal, merge the queue entries
      if (next != null && current.reactive == next.reactive) {
        next.minLevel = next.minLevel max current.minLevel
        next.needsEvaluate ||= current.needsEvaluate
      }
      else {
        handleElement(current)
      }
      current = elements.poll()
      next = elements.peek()
    }

  }


  /**
    * Removes a reactive element from the queue
    *
    * @param reactive Element to remove from the queue
    */
  def remove(reactive: Reactive[S]): Unit = {
    val it = elements.iterator()
    while (it.hasNext) if (it.next().reactive eq reactive) it.remove()
  }
}


private[levelbased] object LevelQueue {

  trait Evaluator[S <: Struct] {
    def evaluate(r: Reactive[S]): Unit
  }

  private final case class QueueElement[S <: Struct](level: Int, reactive: Reactive[S], var minLevel: Int, var needsEvaluate: Boolean) extends Comparable[QueueElement[S]] {
    // order by level, then by reactive
    val order: Long = (level.toLong << 32) | (reactive.hashCode.toLong & 0x00000000ffffffffl)
    override def compareTo(o: QueueElement[S]): Int = java.lang.Long.compare(order, o.order)
  }
}
