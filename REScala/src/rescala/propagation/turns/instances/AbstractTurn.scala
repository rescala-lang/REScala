package rescala.propagation.turns.instances

import rescala.propagation.EvaluationResult.{Dynamic, Static}
import rescala.propagation.Reactive
import rescala.propagation.turns.Turn

abstract class AbstractTurn extends Turn {
  outer =>
  implicit def currentTurn: AbstractTurn = this

  protected var toCommit = Set[Reactive]()
  protected var afterCommitHandlers = List[() => Unit]()

  protected var initialSources: List[Reactive] = Nil

  def ensureLevel(dependant: Reactive, dependencies: Set[Reactive]): Int =
    if (dependencies.isEmpty) 0
    else {
      val newLevel = dependencies.map(_.level.get).max + 1
      dependant.level.transform(math.max(newLevel, _))
    }

  val dependencyManagement = new DependencyManagement()

  val levelQueue = new LevelQueue(this.evaluate)


  def isReady(reactive: Reactive, dependencies: Set[Reactive]) =
    dependencies.forall(_.level.get < reactive.level.get)

  /** removes reactive from its dependencies */
  override def unregister(dependant: Reactive)(dependency: Reactive): Unit = dependencyManagement.unregister(dependant)(dependency)


  /** evaluates a single reactive */
  def evaluate(head: Reactive): Unit = {
    val result = head.reevaluate()
    val (headChanged, newHeadLevel) = result match {
      case Static(hasChanged) => (hasChanged, -42)
      case diff@Dynamic(hasChanged, newDependencies, oldDependencies) =>
        dependencyManagement.handleDiff(head, newDependencies, oldDependencies)
        val newLevel = ensureLevel(head, newDependencies)
        if (isReady(head, newDependencies)) {
          (hasChanged, newLevel)
        }
        else {
          levelQueue.enqueue(newLevel)(head)
          (false, newLevel)
        }
    }
    if (headChanged) {
      head.dependants.get.foreach(levelQueue.enqueue(newHeadLevel + 1))
    }
  }


  def propagationPhase(): Unit = {
    initialSources.foreach(levelQueue.enqueue(0))
    levelQueue.evaluateQueue()
  }

  def markForCommit(reactive: Reactive): Unit = {
    toCommit += reactive
  }

  def commitPhase() = toCommit.foreach(_.commit(this))

  override def afterCommit(handler: => Unit) = afterCommitHandlers ::= handler _

  def observerPhase() = afterCommitHandlers.foreach(_())

  def create[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    val reactive = f
    dependencies.foreach(dependencyManagement.register(reactive))
    ensureLevel(reactive, dependencies)
    reactive
  }

  def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    val reactive = f
    ensureLevel(reactive, dependencies)
    evaluate(reactive)
    reactive
  }

  def acquireDynamic(reactive: Reactive): Unit

  /** admits a new source change */
  override def admit(source: Reactive)(setPulse: => Boolean): Unit = if(setPulse) initialSources ::= source

  def lockingPhase(): Unit
  def realeasePhase(): Unit
}