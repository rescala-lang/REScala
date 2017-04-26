package rescala.levelbased

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph.{Change, Reactive, ReevaluationResult}
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

  override protected def ignite[T <: Reactive[S]](reactive: T, incomingOrDynamic: Option[Set[Reactive[S]]]): Unit = {
    incomingOrDynamic match {
      case Some(incoming) =>
        val level = incoming.foldLeft(0) { (maxLevel, dep) =>
          dynamicDependencyInteraction(dep)
          discover(reactive)(dep)
          math.max(maxLevel, dep.state.level(this))
        }

        reactive.state.updateLevel(level)(this)

        if (level <= levelQueue.currentLevel() && incoming.exists(_evaluated.contains)) {
          // TODO evaluate if level <= levelQueue.current or enqueue otherwise, rather than possible double reevaluation?
          evaluate(reactive)
        }
      case None =>
        evaluate(reactive)
    }
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
