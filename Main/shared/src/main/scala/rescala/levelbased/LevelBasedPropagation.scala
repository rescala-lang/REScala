package rescala.levelbased

import rescala.core.{Reactive, ReevaluationResult}
import rescala.core.ReevaluationResult.{Dynamic, Static}
import rescala.twoversion.TwoVersionPropagationImpl

/**
  * Further implementation of level-based propagation based on the common propagation implementation.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait LevelBasedPropagation[S <: LevelStruct] extends TwoVersionPropagationImpl[S] with LevelQueue.Evaluator[S] {
  private var _evaluated = List.empty[Reactive[S]]

  val levelQueue = new LevelQueue[S](this)(this)

  override def evaluate(head: Reactive[S]): Unit = {
    def reevOut(level: Int, res: ReevaluationResult[head.Value, S]) = {
      if (res.isChange) {
        writeState(head)(res.value)
        head.state.outgoing(this).foreach(levelQueue.enqueue(level, needsEvaluate = true))
      }
    }

    head.reevaluate(this, head.state.base(token), head.state.incoming(this)) match {
      case res: Static[head.Value] => reevOut(-42, res)
      case res: Dynamic[head.Value, S] =>
        val newLevel = maximumLevel(res.indepsAfter) + 1
        val redo = head.state.level(this) < newLevel
        if(redo) {
          levelQueue.enqueue(newLevel, needsEvaluate = true)(head)
        } else {
          applyDiff(head, res)
          reevOut(newLevel, res)
        }
    }
    _evaluated ::= head

  }

  private def maximumLevel(dependencies: Set[Reactive[S]]): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.state.level(this)))

  override protected def ignite(reactive: Reactive[S], incoming: Set[Reactive[S]], ignitionRequiresReevaluation: Boolean): Unit = {
    val level = if(incoming.isEmpty) 0 else incoming.map(_.state.level(this)).max + 1
    reactive.state.updateLevel(level)(this)

    incoming.foreach { dep =>
      dynamicDependencyInteraction(dep)
      discover(reactive)(dep)
    }
    reactive.state.updateIncoming(incoming)(this)

    if(ignitionRequiresReevaluation || incoming.exists(_evaluated.contains)) {
      if (level <= levelQueue.currentLevel()) {
        evaluate(reactive)
      } else {
        levelQueue.enqueue(level)(reactive)
      }
    }
  }

  override def preparationPhase(initialWrites: Traversable[Reactive[S]]): Unit = {
    initialWrites.foreach { reactive =>
      levelQueue.enqueue(reactive.state.level(this))(reactive)
    }
  }

  def propagationPhase(): Unit = levelQueue.evaluateQueue()
}
