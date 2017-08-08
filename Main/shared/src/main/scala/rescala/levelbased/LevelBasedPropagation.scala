package rescala.levelbased

import rescala.core.Node.InDep
import rescala.core.Reactive
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
    if(!res.indepsChanged) {
      // res.commitValueChange().foreach(levelQueue.enqueue(-42))
      if(res.valueChanged) {
        writeState(res.commitTuple)
        head.state.outgoing(this).foreach(levelQueue.enqueue(-42))
      }
    } else {
      val newLevel = maximumLevel(res.indepsAfter) + 1
      val redo = head.state.level(this) < newLevel
      if(redo) {
        levelQueue.enqueue(newLevel)(head)
      } else {
        res.commitDependencyDiff()

        // res.commitValueChange().foreach(levelQueue.enqueue(-42))
        if(res.valueChanged) {
          writeState(res.commitTuple)
          head.state.outgoing(this).foreach(levelQueue.enqueue(newLevel))
        }
      }
    }

    _evaluated ::= head

  }

  private def maximumLevel(dependencies: Set[InDep[S]]): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.state.level(this)))

  override protected def ignite(reactive: Reactive[S], incoming: Set[InDep[S]], ignitionRequiresReevaluation: Boolean): Unit = {
    val level = if(incoming.isEmpty) 0 else incoming.map(_.state.level(this)).max + 1
    reactive.state.updateLevel(level)(this)

    incoming.foreach { dep =>
      dynamicDependencyInteraction(dep)
      discover(dep, reactive)
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
