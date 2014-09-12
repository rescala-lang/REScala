package rescala.propagation

import rescala.signals.VarSynt
import rescala.{Dependency, Reactive, Dependant}
import rescala.log.ReactiveLogging

import scala.collection.mutable

/**
 * The engine that schedules the (glitch-free) evaluation
 * of the nodes in the dependency graph.
 */
class Turn extends ReactiveLogging {
  private val evalQueue = new mutable.PriorityQueue[(Int, Dependant)]()(Turn.reactiveOrdering)
  private var evaluated = List[Dependant]()

  /** Adds a dependant to the eval queue */
  def addToEvalQueue(dep: Dependant): Unit = {
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
      if (level == head.level) {
        head.triggerReevaluation()(this)
        evaluated ::= head
      }
      else addToEvalQueue(head)
    }
    evaluated.foreach(_.applyPulse(this))
  }
}


object Turn {
  val currentTurn = new scala.util.DynamicVariable[Option[Turn]](None)

  def maybeTurn[T](f: Turn => T) = currentTurn.value match {
    case None => newTurn(f)
    case Some(turn) => f(turn)
  }

  def newTurn[T](f: Turn => T) = synchronized {
    val turn = new Turn
    currentTurn.withValue(Some(turn)){f(turn)}
  }

  val reactiveOrdering = new Ordering[(Int, Dependant)] {
    override def compare(x: (Int, Dependant), y: (Int, Dependant)): Int = y._1.compareTo(x._1)
  }
}
