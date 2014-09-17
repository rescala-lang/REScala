package rescala.propagation

import rescala.propagation.EvaluationResult.{Retry, Done}
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
  private val evalQueue = new mutable.PriorityQueue[(Int, Reactive)]()(Turn.reactiveOrdering)
  private var evaluated = List[Reactive]()

  /** Adds a dependant to the eval queue */
  def evaluate(dep: Reactive): Unit = {
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
      if (level != head.level) evaluate(head)
      else {
        head.reevaluate()(this) match {
          case Done(dependants) =>
            dependants.foreach(evaluate)
            evaluated ::= head
          case Retry =>
            evaluate(head)
        }
      }
    }
    evaluated.foreach(_.commit(this))
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

  val reactiveOrdering = new Ordering[(Int, Reactive)] {
    override def compare(x: (Int, Reactive), y: (Int, Reactive)): Int = y._1.compareTo(x._1)
  }
}
