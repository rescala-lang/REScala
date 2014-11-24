package rescala.propagation.turns.instances

import rescala.propagation.EvaluationResult.{Dynamic, Static}
import rescala.propagation.Reactive
import rescala.propagation.turns.Turn
import rescala.propagation.turns.creation.TurnFactory

import scala.annotation.tailrec
import scala.collection.mutable

abstract class AbstractTurn extends Turn {
  outer =>
  implicit def currentTurn: Turn = this

  protected var toCommit = Set[Reactive]()
  protected var afterCommitHandlers = List[() => Unit]()

  protected var initialSources: List[Reactive] = Nil

  val levelQueue = new LevelQueue() {
    override def handleDiff(dependant: Reactive, newDependencies: Set[Reactive], oldDependencies: Set[Reactive]): Unit =
      outer.handleDiff(dependant,newDependencies, oldDependencies)
  }


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

  def evaluate(reactive: Reactive): Unit = levelQueue.evaluate(reactive)

  def propagationPhase(): Unit = {
    initialSources.foreach(levelQueue.enqueue)
    levelQueue.propagationPhase()
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
  override def admit(source: Reactive)(setPulse: => Boolean): Unit = if(setPulse) initialSources ::= source

  def lockingPhase(): Unit
  def realeasePhase(): Unit
}