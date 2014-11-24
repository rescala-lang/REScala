package rescala.propagation.turns.instances

import rescala.propagation.EvaluationResult.{Dynamic, Static}
import rescala.propagation.Reactive
import rescala.propagation.turns.Turn

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.DynamicVariable

abstract class AbstractTurn extends Turn {

  protected val evalQueue = new mutable.PriorityQueue[(Int, Reactive)]()(Synchronized.reactiveOrdering)
  protected var toCommit = Set[Reactive]()
  protected var afterCommitHandlers = List[() => Unit]()

  implicit def implicitThis: Turn = this

  def register(dependant: Reactive)(dependency: Reactive): Unit = {
    dependency.dependants.transform(_ + dependant)
  }

  def ensureLevel(dependant: Reactive, dependencies: Set[Reactive]): Boolean =
    if (dependencies.nonEmpty) setLevelIfHigher(dependant, dependencies.map(_.level.get).max + 1)
    else false

  def setLevelIfHigher(reactive: Reactive, level: Int): Boolean = {
    reactive.level.transform { case x if x < level => level }
  }

  override def unregister(dependant: Reactive)(dependency: Reactive): Unit = {
    acquireDynamic(dependency)
    dependency.dependants.transform(_ - dependant)
  }

  def handleDiff(dependant: Reactive,newDependencies: Set[Reactive] , oldDependencies: Set[Reactive]): Unit = {
    newDependencies.foreach(acquireDynamic)

    val removedDependencies = oldDependencies.diff(newDependencies)
    removedDependencies.foreach(unregister(dependant))

    val addedDependencies = newDependencies.diff(oldDependencies)
    addedDependencies.foreach(register(dependant))

    ensureLevel(dependant, addedDependencies)
  }


  def isReady(reactive: Reactive, dependencies: Set[Reactive]) =
    dependencies.forall(_.level.get < reactive.level.get)

  @tailrec
  protected final def floodLevel(reactives: Set[Reactive]): Unit =
    if (reactives.nonEmpty) {
      val reactive = reactives.head
      val level = reactive.level.get + 1
      val dependants = reactive.dependants.get
      val changedDependants = dependants.filter(setLevelIfHigher(_, level))
      floodLevel(reactives.tail ++ changedDependants)
    }

  /** mark the reactive as needing a reevaluation */
  def enqueue(dep: Reactive): Unit = {
    if (!evalQueue.exists { case (_, elem) => elem eq dep }) {
      evalQueue.+=((dep.level.get, dep))
    }
  }


  /** evaluates a single reactive */
  def evaluate(head: Reactive): Unit = {
    val result = head.reevaluate()(this)
    val headChanged = result match {
      case Static(hasChanged) => hasChanged
      case diff@Dynamic(hasChanged, newDependencies, oldDependencies) =>
        handleDiff(head, newDependencies, oldDependencies)
        if (isReady(head, newDependencies)) {
          floodLevel(Set(head))
          hasChanged
        }
        else {
          enqueue(head)
          false
        }
    }
    if (headChanged) {
      head.dependants.get.foreach(enqueue)
    }

  }

  /** Evaluates all the elements in the queue */
  def propagationPhase() = {
    while (evalQueue.nonEmpty) {
      val (level, head) = evalQueue.dequeue()
      // check the level if it changed queue again
      if (level != head.level.get) enqueue(head)
      else evaluate(head)
    }
  }

  def markForCommit(reactive: Reactive): Unit = {
    toCommit += reactive
  }

  def commitPhase() = toCommit.foreach(_.commit(this))

  override def afterCommit(handler: => Unit) = afterCommitHandlers ::= handler _

  def observerPhase() = afterCommitHandlers.foreach(_())

  override def create[T <: Reactive](dependencies: Set[Reactive])(f: => T): T

  override def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T

  def acquireDynamic(reactive: Reactive): Unit

  /** admits a new source change */
  override def admit(source: Reactive)(setPulse: => Boolean): Unit = if(setPulse) enqueue(source)

  def lockingPhase(): Unit
  def realeasePhase(): Unit
}