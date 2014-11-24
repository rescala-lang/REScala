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

  val levelManipulation = new LevelManipulation()
  val dependencyManagement = new DependencyManagement()

  val levelQueue = new LevelQueue() {
    /** evaluates a single reactive */
    override def evaluate(head: Reactive): Unit = outer.evaluate(head)
  }


  def isReady(reactive: Reactive, dependencies: Set[Reactive]) =
    dependencies.forall(_.level.get < reactive.level.get)

  /** removes reactive from its dependencies */
  override def unregister(dependant: Reactive)(dependency: Reactive): Unit = dependencyManagement.unregister(dependant)(dependency)

  /** evaluates a single reactive */
  def evaluate(head: Reactive): Unit = {
    val result = head.reevaluate()
    val headChanged = result match {
      case Static(hasChanged) => hasChanged
      case diff@Dynamic(hasChanged, newDependencies, oldDependencies) =>
        dependencyManagement.handleDiff(head, newDependencies, oldDependencies)
        if (isReady(head, newDependencies)) {
          levelManipulation.floodLevel(Set(head))
          hasChanged
        }
        else {
          levelQueue.enqueue(head)
          false
        }
    }
    if (headChanged) {
      head.dependants.get.foreach(levelQueue.enqueue)
    }

  }


  def propagationPhase(): Unit = {
    initialSources.foreach(levelQueue.enqueue)
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
    levelManipulation.ensureLevel(reactive, dependencies)
    reactive
  }

  def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    val reactive = f
    levelManipulation.ensureLevel(reactive, dependencies)
    evaluate(reactive)
    reactive
  }

  def acquireDynamic(reactive: Reactive): Unit

  /** admits a new source change */
  override def admit(source: Reactive)(setPulse: => Boolean): Unit = if(setPulse) initialSources ::= source

  def lockingPhase(): Unit
  def realeasePhase(): Unit
}