package rescala.propagation

import java.lang.{ Boolean => jlBool }

import rescala.graph.{Spores, Reactive}
import rescala.propagation.LevelQueue.QueueElement
import rescala.turns.Turn

import scala.collection.immutable.SortedSet

class LevelQueue[S <: Spores]()(implicit val currenTurn: Turn[S]) {

  private var elements = SortedSet.empty[QueueElement[S]]
  private var numOccurences = Map[Reactive[S], Int]()

  def currentLevel(): Int = elements.headOption.fold(Int.MaxValue)(_.level)

  /** mark the reactive as needing a reevaluation */
  def enqueue(minLevel: Int, needsEvaluate: Boolean = true)(dep: Reactive[S]): Unit = {
    val newElem = QueueElement[S](dep.bud.level, dep, minLevel, needsEvaluate)
    if (!elements.contains(newElem)) {

      elements += newElem
      numOccurences = numOccurences + (dep -> (numOccurences.getOrElse(dep, 0) + 1))
    }
  }

  def remove(reactive: Reactive[S]): Unit = {
    elements = elements.filter(qe => qe.reactive ne reactive)
    numOccurences = numOccurences - reactive
  }


  def isEmpty() = this.synchronized {elements.isEmpty}

  final def handleHead(queueElement: QueueElement[S], evaluator: Reactive[S] => Unit, notEvaluator: Reactive[S] => Unit): () => Unit = {
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
      head.outgoing.foreach { r =>
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

  def clear() = elements = SortedSet[QueueElement[S]]()

}

object LevelQueue {

  private case class QueueElement[S <: Spores](level: Int, reactive: Reactive[S], minLevel: Int, needsEvaluate: Boolean)
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
  private implicit def lqo[S <: Spores]: Ordering[QueueElement[S]] = ordering.asInstanceOf[Ordering[QueueElement[S]]]
}

