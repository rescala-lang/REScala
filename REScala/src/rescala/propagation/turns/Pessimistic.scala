package rescala.propagation.turns

import rescala.propagation.EvaluationResult.{Done, DependencyDiff}
import rescala.propagation.{TurnFactory, Turn, Reactive}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.DynamicVariable

object Pessimistic extends TurnFactory {

  val currentTurn: DynamicVariable[Option[Turn]] = new DynamicVariable[Option[Turn]](None)

  override def maybeDynamicTurn[T](f: (Turn) => T): T = currentTurn.value match {
    case None => newTurn(f)
    case Some(turn) => f(turn)
  }

  def reachable(reactives: Set[Reactive])(implicit turn: Turn) =
    reactives ++ reactives.flatMap(_.dependants.get)

  def lock(reactives: List[Reactive]): Unit = reactives.sortBy(r => System.identityHashCode(r.lock)).foreach(_.lock.lock())

  def acquireLocks(reactives: List[Reactive])(implicit turn: Turn): Set[Reactive] = {
    lock(reactives)
    val reached = reachable(reactives.toSet)(turn)
    lock(reached.toList)
    reached
  }

  override def newTurn[T](f: Turn => T): T = {
    val turn = new Pessimistic()
    val result = currentTurn.withValue(Some(turn)) {
      val res = f(turn)
      val locked = acquireLocks(turn.evalQueue.map(_._2).toList)(turn)
      try {
        turn.evaluateQueue()
        turn.commit()
      } finally {
        locked.foreach(_.lock.unlock())
      }
      res
    }
    turn.runAfterCommitHandlers()
    result
  }

  val reactiveOrdering = new Ordering[(Int, Reactive)] {
    override def compare(x: (Int, Reactive), y: (Int, Reactive)): Int = y._1.compareTo(x._1)
  }

}

class Pessimistic extends Turn {
  private val evalQueue = new mutable.PriorityQueue[(Int, Reactive)]()(Synchronized.reactiveOrdering)
  private var toCommit = Set[Reactive]()
  private var afterCommitHandlers = List[() => Unit]()

  implicit def implicitThis: Turn = this

  def register(dependant: Reactive, dependencies: Set[Reactive]): Unit = {
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
    reactive.level.transform { case x if x < level => level }
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
        if (hasChanged) head.dependants.get.foreach(enqueue)
        dependencyDiff.foreach(handleDiff(head, _))
        changed(head)
      case diff@DependencyDiff(_, _) =>
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

  def afterCommit(handler: => Unit) = afterCommitHandlers ::= handler _

  def runAfterCommitHandlers() = afterCommitHandlers.foreach(_())

  val bag = new DynamicVariable(Set[Reactive]())
  override def collectDependencies[T](f: => T): (T, Set[Reactive]) = bag.withValue(Set()) { (f, bag.value) }
  override def useDependency(dependency: Reactive): Unit = bag.value = bag.value + dependency

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

