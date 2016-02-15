package rescala.pipelining.propagation

import java.lang.{ Boolean => jlBool }

import rescala.graph.Reactive
import rescala.pipelining.PipelineSpores
import rescala.pipelining.propagation.PipelineQueue.QueueElement
import rescala.turns.Turn

import scala.collection.SortedSet

class PipelineQueue()(implicit val currentTurn: Turn[PipelineSpores.type]) {

  type S = PipelineSpores.type

  private var elements = SortedSet[QueueElement]()
  private var numOccurences = Map[Reactive[S], Int]()

  /** mark the reactive as needing a reevaluation */
  def enqueue(minLevel: Int, needsEvaluate: Boolean = true)(dep: Reactive[S]): Unit = this.synchronized {
    val newElem = QueueElement(dep.bud.level, dep, minLevel, needsEvaluate)
    if (!elements.contains(newElem)) {
      elements += newElem
      numOccurences = numOccurences + (dep -> (numOccurences.getOrElse(dep, 0) + 1))
    }
  }

  def remove(reactive: Reactive[S]): Unit = this.synchronized {
    elements = elements.filter(qe => qe.reactive ne reactive) // THat is wrong
    numOccurences = numOccurences - reactive
  }

  def isEmpty() = this.synchronized {elements.isEmpty}

  final def handleHead(queueElement: QueueElement, evaluator: Reactive[S] => Unit, notEvaluator: Reactive[S] => Unit): () => Unit = {
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
      () => {}
    } else if (doEvaluate) {
      () => evaluator(head)
    } else if (numOccurences(head) == 1) {
      () => notEvaluator(head)
    } else {
      () => {}
    }
  }

  /** Evaluates all the elements in the queue */
  def evaluateQueue(evaluator: Reactive[S] => Unit, notEvaluator: Reactive[S] => Unit = r => {}) = {
    while (elements.nonEmpty) {
      this.synchronized {

        val head = elements.head
        elements = elements.tail
        val queueAction = handleHead(head, evaluator, notEvaluator)
        val numOccurence = numOccurences(head.reactive)
        if (numOccurence == 1)
          numOccurences -= head.reactive
        else numOccurences += (head.reactive -> (numOccurence - 1))
        queueAction
      } ()
    }
  }

  def clear() = elements = SortedSet[QueueElement]()

}

object PipelineQueue {

  private case class QueueElement(level: Int, reactive: Reactive[PipelineSpores.type], minLevel: Int, needsEvaluate: Boolean)
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
