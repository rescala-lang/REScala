package rescala.propagation

import rescala.signals.Var
import rescala.{Dependency, Reactive, Dependant}
import rescala.log.ReactiveLogging

import scala.collection.mutable
import scala.util.DynamicVariable

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

  object dynamic {
    val bag = new DynamicVariable(Set[Dependency[_]]())
    def used(dependency: Dependency[_]) = bag.value = bag.value + dependency
  }

}


object Turn {
  val currentTurn = new DynamicVariable[Option[Turn]](None)

  def maybeTurn[T](f: Turn => T)(implicit maybe: MaybeTurn) = maybe.turn match {
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
