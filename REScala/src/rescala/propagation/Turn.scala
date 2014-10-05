package rescala.propagation

import rescala.propagation.EvaluationResult.{Done, DependencyDiff}

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

  implicit def implicitThis: Turn = this

  def register(dependant: Reactive, dependencies: Set[Reactive]): Unit =  {
    dependencies.foreach { dependency =>
      dependency.dependants.transform(_ + dependant)
      changed(dependency)
    }
    ensureLevel(dependant, dependencies)
  }

  def ensureLevel(dependant: Reactive, dependencies: Set[Reactive]): Unit =
    if (dependencies.nonEmpty) {
      ensureLevel(dependant, dependencies.map(_.level.get).max + 1)
      changed(dependant)
    }

  def ensureLevel(reactive: Reactive, level: Int): Boolean = {
    reactive.level.transform { case x if x < level => level}
  }

  def unregister(dependant: Reactive, dependencies: Set[Reactive]): Unit = dependencies.foreach { dependency =>
    dependency.dependants.transform(_ - dependant)
    changed(dependency)
  }

  def handleDiff(dependant: Reactive, diff: DependencyDiff): Unit = {
    val DependencyDiff(newDependencies, oldDependencies) = diff
    unregister(dependant, oldDependencies.diff(newDependencies))
    register(dependant, newDependencies.diff(oldDependencies))
  }

  def isReady(reactive: Reactive, dependencies: Set[Reactive]) =
    dependencies.forall(_.level.get < reactive.level.get)

  @tailrec
  private def floodLevel(reactives: Set[Reactive]): Unit =
    if (reactives.nonEmpty) {
      val reactive = reactives.head
      changed(reactive)
      val level = reactive.level.get + 1
      val dependants = reactive.dependants.get
      val changedDependants = dependants.filter(ensureLevel(_, level))
      floodLevel(reactives.tail ++ changedDependants)
    }

  /** Adds a dependant to the eval queue */
  def enqueue(dep: Reactive): Unit = {
      if (!evalQueue.exists { case (_, elem) => elem eq dep }) {
        evalQueue.+=((dep.level.get, dep))
      }
  }


  /** evaluates a single reactive */
  def evaluate(head: Reactive): Unit = {
    val result = head.reevaluate()(this)
    result match {
      case Done(hasChanged, dependencyDiff) =>
        if(hasChanged) head.dependants.get.foreach(enqueue)
        dependencyDiff.foreach(handleDiff(head, _))
        changed(head)
      case diff @ DependencyDiff(_, _) =>
        handleDiff(head, diff)
        floodLevel(Set(head))
        enqueue(head)
    }
  }

  /** Evaluates all the elements in the queue */
  def evaluateQueue() = {
    while (evalQueue.nonEmpty) {
      val (level, head) = evalQueue.dequeue()
      // check the level if it changed queue again
      if (level != head.level.get) enqueue(head)
      else evaluate(head)
    }
  }

  def changed(reactive: Reactive): Unit = toCommit += reactive

  def commit() = toCommit.foreach(_.commit(this))

  object dynamic {
    val bag = new DynamicVariable(Set[Reactive]())
    def used(dependency: Reactive) = bag.value = bag.value + dependency
  }

  def create[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    val reactive = f
    register(reactive, dependencies)
    reactive
  }

  def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    val reactive = f
    ensureLevel(reactive, dependencies)
    evaluate(reactive)
    reactive
  }
}


object Turn {
  val currentTurn = new DynamicVariable[Option[Turn]](None)

  def maybeTurn[T](f: Turn => T)(implicit maybe: MaybeTurn): T = maybe.turn match {
    case None => newTurn(f)
    case Some(turn) => f(turn)
  }

  def newTurn[T](f: Turn => T): T = synchronized {
    val turn = new Turn
    val result = currentTurn.withValue(Some(turn)){
      val res = f(turn)
      turn.evaluateQueue()
      turn.commit()
      res
    }
    result
  }

  val reactiveOrdering = new Ordering[(Int, Reactive)] {
    override def compare(x: (Int, Reactive), y: (Int, Reactive)): Int = y._1.compareTo(x._1)
  }
}
