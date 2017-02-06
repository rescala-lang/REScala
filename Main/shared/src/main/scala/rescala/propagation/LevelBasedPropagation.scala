package rescala.propagation

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph.{LevelStruct, Reactive}

/**
  * Further implementation of level-based propagation based on the common propagation implementation.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait LevelBasedPropagation[S <: LevelStruct] extends CommonPropagationImpl[S] with LevelQueue.Evaluator[S] {


  implicit def currentTurn: LevelBasedPropagation[S] = this


  private var _evaluated = List.empty[Reactive[S]]

  val levelQueue = new LevelQueue[S](this)

  def evaluate(head: Reactive[S]): Unit = {

    def requeue(changed: Boolean, level: Int, redo: Boolean): Unit =
      if (redo) levelQueue.enqueue(level, changed)(head)
      else if (changed) head.bud.outgoing.foreach(levelQueue.enqueue(level, changed))

    head.reevaluate() match {
      case Static(hasChanged) =>
        requeue(hasChanged, level = -42, redo = false)
      case Dynamic(hasChanged, diff) =>
        applyDiff(head, diff)
        val newLevel = maximumLevel(diff.novel) + 1
        requeue(hasChanged, newLevel, redo = head.bud.level < newLevel)
    }
    _evaluated ::= head

  }

  private def maximumLevel(dependencies: Set[Reactive[S]]): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.bud.level))


  override def create[T <: Reactive[S]](dependencies: Set[Reactive[S]], dynamic: Boolean)(f: => T): T = {
    val reactive = f
    val level = ensureLevel(reactive, dependencies)
    if (dynamic) evaluate(reactive)
    else {
      dependencies.foreach(discover(reactive))
      if (level <= levelQueue.currentLevel() && dependencies.exists(_evaluated.contains)) {
        evaluate(reactive)
      }
    }
    reactive
  }

  def ensureLevel(dependant: Reactive[S], dependencies: Set[Reactive[S]]): Int =
    if (dependencies.isEmpty) 0
    else {
      val newLevel = dependencies.map(_.bud.level).max + 1
      dependant.bud.updateLevel(newLevel)
    }

  /** allow turn to handle dynamic access to reactives */
  override def dynamicDependencyInteraction(dependency: Reactive[S]): Unit = ()

  override def preparationPhase(initialWrites: Traversable[Reactive[S]]): Unit = initialWrites.foreach { reactive =>
    levelQueue.enqueue(reactive.bud.level)(reactive)
  }

  def propagationPhase(): Unit = levelQueue.evaluateQueue()


}
