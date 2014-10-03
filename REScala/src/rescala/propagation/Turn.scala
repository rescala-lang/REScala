package rescala.propagation

import rescala.propagation.EvaluationResult.{Done, Retry}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.DynamicVariable

/**
 * The engine that schedules the (glitch-free) evaluation
 * of the nodes in the dependency graph.
 */
class Turn {
  private val evalQueue = new mutable.PriorityQueue[(Int, Reactive)]()(Turn.reactiveOrdering)
  private var toCommit = Set[Reactive]()

  implicit def turn: Turn = this

  def register(dependant: Reactive, dependencies: Set[Reactive]): Unit =  {
    dependencies.foreach(_.addDependant(dependant))
    if (dependencies.nonEmpty) {
      dependant.ensureLevel(dependencies.map(_.level).max + 1)
      changed(dependant)
    }
  }

  def unregister(dependant: Reactive, dependencies: Set[Reactive]): Unit = dependencies.foreach(_.removeDependant(dependant))

  def isReady(reactive: Reactive, dependencies: Set[Reactive]) =
    dependencies.forall(_.level < reactive.level)

  @tailrec
  private def floodLevel(reactives: Set[Reactive]): Unit =
    if (reactives.nonEmpty) {
      val reactive = reactives.head
      changed(reactive)
      val level = reactive.level + 1
      val dependants = reactive.dependants
      val changedDependants = dependants.filter(_.ensureLevel(level))
      floodLevel(reactives.tail ++ changedDependants)
    }

  /** Adds a dependant to the eval queue */
  def enqueue(dep: Reactive): Unit = {
      if (!evalQueue.exists { case (_, elem) => elem eq dep }) {
        evalQueue.+=((dep.level, dep))
      }
  }

  /** evaluates a single reactive */
  def evaluate(head: Reactive): Unit = {
    val result = head.reevaluate()(this)
    result match {
      case Done(hasChanged, dependants, newDependencies) =>
        if(hasChanged) dependants.foreach(enqueue)
        register(head, newDependencies)
        changed(head)
      case Retry(dependencies) =>
        register(head, dependencies)
        floodLevel(Set(head))
        enqueue(head)
    }
  }

  /** Evaluates all the elements in the queue */
  def evaluateQueue() = {
    while (evalQueue.nonEmpty) {
      val (level, head) = evalQueue.dequeue()
      // check the level if it changed queue again
      if (level != head.level) enqueue(head)
      else evaluate(head)
    }
  }

  def changed(reactive: Reactive): Unit = toCommit += reactive

  def commit() = toCommit.foreach(_.commit(this))

  object dynamic {
    val bag = new DynamicVariable(Set[Reactive]())
    def used(dependency: Reactive) = bag.value = bag.value + dependency
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
    val res = currentTurn.withValue(Some(turn)){f(turn)}
    turn.evaluateQueue()
    turn.commit()
    res
  }

  val reactiveOrdering = new Ordering[(Int, Reactive)] {
    override def compare(x: (Int, Reactive), y: (Int, Reactive)): Int = y._1.compareTo(x._1)
  }
}
