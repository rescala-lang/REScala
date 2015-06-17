package rescala.propagation

import java.lang.{ Boolean => jlBool }

import rescala.graph.Reactive
import rescala.propagation.LevelQueue.QueueElement
import rescala.turns.Turn

import scala.collection.SortedSet

class LevelQueue()(implicit val currentTurn: Turn) {

  private var elements = SortedSet[QueueElement]()
  private var numOccurences = Map[Reactive, Int]()

  /** mark the reactive as needing a reevaluation */
  def enqueue(minLevel: Int, needsEvaluate: Boolean = true)(dep: Reactive): Unit = this.synchronized {
    val newElem = QueueElement(dep.level.get, dep, minLevel, needsEvaluate)
    if (!elements.contains(newElem)) {
      elements += newElem
      numOccurences = numOccurences + (dep -> (numOccurences.getOrElse(dep, 0) + 1))
    }
  }

  def remove(reactive: Reactive): Unit = this.synchronized {
    elements = elements.filter(qe => qe.reactive ne reactive) // THat is wrong
    numOccurences = numOccurences - reactive
  }
  
  def isEmpty() = this.synchronized {elements.isEmpty}

  final def handleHead(queueElement: QueueElement, evaluator: Reactive => Unit, notEvaluator: Reactive => Unit): () => Unit = {
    val QueueElement(headLevel, head, headMinLevel, doEvaluate) = queueElement
    if (headLevel < headMinLevel) {
      head.level.set(headMinLevel)
      val reevaluate = if (doEvaluate) true
      else if (elements.isEmpty) false
      else if (elements.head.reactive ne head) false
      else {
        elements = elements.tail
        true
      }
      enqueue(headMinLevel, reevaluate)(head)
      head.outgoing.get.foreach { r =>
        if (r.level.get <= headMinLevel)
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
  def evaluateQueue(evaluator: Reactive => Unit, notEvaluator: Reactive => Unit = r => {}) = {
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

object LevelQueue {

  private case class QueueElement(level: Int, reactive: Reactive, minLevel: Int, needsEvaluate: Boolean)
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

