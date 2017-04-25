package rescala.levelbased

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph.{Reactive, ReevaluationResult}
import rescala.twoversion.CommonPropagationImpl

/**
  * Further implementation of level-based propagation based on the common propagation implementation.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait LevelBasedPropagation[S <: LevelStruct] extends CommonPropagationImpl[S] with LevelQueue.Evaluator[S] {
  private var _evaluated = List.empty[Reactive[S]]

  val levelQueue = new LevelQueue[S](this)(this)

  override def evaluate(head: Reactive[S]): Unit = {
    def reevOut(level: Int, res: ReevaluationResult[head.Value, S]) = {
      if (res.isChange) {
        writeState(head)(res.value)
        head.state.outgoing(this).foreach(levelQueue.enqueue(level, true))
      }
    }

    head.reevaluate(this) match {
      case res: Static[head.Value] => reevOut(-42, res)
      case res: Dynamic[head.Value, S] =>
        val newLevel = maximumLevel(res.dependencies) + 1
        val redo = head.state.level(this) < newLevel
        if(redo) {
          levelQueue.enqueue(newLevel, true)(head)
        } else {
          applyDiff(head, res.depDiff(head.state.incoming(this)))
          reevOut(newLevel, res)
        }
    }
    _evaluated ::= head

  }

  private def maximumLevel(dependencies: Set[Reactive[S]]): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.state.level(this)))


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
      val newLevel = dependencies.map(_.state.level(this)).max + 1
      dependant.state.updateLevel(newLevel)(this)
    }

  /** allow turn to handle dynamic access to reactives */
  override def dynamicDependencyInteraction(dependency: Reactive[S]): Unit = ()

  override def preparationPhase(initialWrites: Traversable[Reactive[S]]): Unit = {
    initialWrites.foreach { reactive =>
      levelQueue.enqueue(reactive.state.level(this))(reactive)
    }
  }

  def propagationPhase(): Unit = levelQueue.evaluateQueue()


}
