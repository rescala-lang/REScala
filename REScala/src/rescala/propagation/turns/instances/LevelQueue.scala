package rescala.propagation.turns.instances

import rescala.propagation.EvaluationResult.{Dynamic, Static}
import rescala.propagation.Reactive
import rescala.propagation.turns.Turn
import rescala.propagation.turns.creation.TurnFactory

import scala.annotation.tailrec
import scala.collection.mutable

abstract class LevelQueue()(implicit val currenTurn: Turn) {

  protected val evalQueue = new mutable.PriorityQueue[(Int, Reactive)]()(TurnFactory.reactiveOrdering)

  def handleDiff(dependant: Reactive,newDependencies: Set[Reactive] , oldDependencies: Set[Reactive]): Unit


  def ensureLevel(dependant: Reactive, dependencies: Set[Reactive]): Boolean =
    if (dependencies.nonEmpty) setLevelIfHigher(dependant, dependencies.map(_.level.get).max + 1)
    else false

  def setLevelIfHigher(reactive: Reactive, level: Int): Boolean = {
    reactive.level.transform { case x if x < level => level }
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
    val result = head.reevaluate()
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

}
