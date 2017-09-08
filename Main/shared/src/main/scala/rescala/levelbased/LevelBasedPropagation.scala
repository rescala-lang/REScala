package rescala.levelbased

import rescala.core.{InitialChange, Reactive, ReevaluationResult}
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
    val res = head.reevaluate(this, head.state.base(token), head.state.incoming(this))
    applyResult(head)(res)
  }

  private def applyResult(head: Reactive[S])(res: ReevaluationResult[head.Value, S]) = {
    if (!res.indepsChanged) {
      // res.commitValueChange().foreach(levelQueue.enqueue(-42))
      if (res.valueChanged) {
        writeState(head)(res.value)
        head.state.outgoing(this).foreach(levelQueue.enqueue(-42))
      }
    } else {
      val newLevel = maximumLevel(res.indepsAfter) + 1
      val redo = head.state.level(this) < newLevel
      if (redo) {
        levelQueue.enqueue(newLevel)(head)
      } else {
        res.commitDependencyDiff(this, head)

        // res.commitValueChange().foreach(levelQueue.enqueue(-42))
        if (res.valueChanged) {
          writeState(head)(res.value)
          head.state.outgoing(this).foreach(levelQueue.enqueue(newLevel))
        }
      }
    }

    _evaluated ::= head
  }

  private def maximumLevel(dependencies: Set[Reactive[S]]): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.state.level(this)))

  override protected def ignite(reactive: Reactive[S], incoming: Set[Reactive[S]], ignitionRequiresReevaluation: Boolean): Unit = {
    val level = if (incoming.isEmpty) 0 else incoming.map(_.state.level(this)).max + 1
    reactive.state.updateLevel(level)(this)

    incoming.foreach { dep =>
      dynamicDependencyInteraction(dep)
      discover(dep, reactive)
    }
    reactive.state.updateIncoming(incoming)(this)

    if (ignitionRequiresReevaluation || incoming.exists(_evaluated.contains)) {
      if (level <= levelQueue.currentLevel()) {
        evaluate(reactive)
      } else {
        levelQueue.enqueue(level)(reactive)
      }
    }
  }

  override def initializationPhase(initialChanges: Seq[InitialChange[S]]): Unit = initialChanges.foreach{ic =>
    applyResult(ic.r)(ic.v(ic.r.state.base(token))) }

  def propagationPhase(): Unit = levelQueue.evaluateQueue()
}
