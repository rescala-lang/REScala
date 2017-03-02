package rescala.levelbased

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph.Reactive
import rescala.twoversion.CommonPropagationImpl

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
      else if (changed) head.state.outgoing.foreach(levelQueue.enqueue(level, changed))

    head.reevaluate() match {
      case Static(value) =>
        val hasChanged = setIfChange(head)(value)
        requeue(hasChanged, level = -42, redo = false)
      case Dynamic(value, diff) =>
        applyDiff(head, diff)
        val newLevel = maximumLevel(diff.novel) + 1
        val hasChanged = setIfChange(head)(value)
        requeue(hasChanged, newLevel, redo = head.state.level < newLevel)
    }
    _evaluated ::= head

  }

  private def maximumLevel(dependencies: Set[Reactive[S]]): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.state.level))


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
      val newLevel = dependencies.map(_.state.level).max + 1
      dependant.state.updateLevel(newLevel)
    }

  /** allow turn to handle dynamic access to reactives */
  override def dynamicDependencyInteraction(dependency: Reactive[S]): Unit = ()

  override def preparationPhase(initialWrites: Traversable[Reactive[S]]): Unit = initialWrites.foreach { reactive =>
    levelQueue.enqueue(reactive.state.level)(reactive)
  }

  def propagationPhase(): Unit = levelQueue.evaluateQueue()


}
