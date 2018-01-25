package rescala.levelbased

import rescala.core.{InitialChange, ReSource, Reactive}
import rescala.twoversion.TwoVersionPropagationImpl

import scala.collection.mutable.ArrayBuffer

/**
  * Further implementation of level-based propagation based on the common propagation implementation.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait LevelBasedPropagation[S <: LevelStruct] extends TwoVersionPropagationImpl[S] with LevelQueue.Evaluator[S] {
  private val _propagating = ArrayBuffer[ReSource[S]]()

  val levelQueue = new LevelQueue[S](this)(this)

  override def clear(): Unit = {
    super.clear()
    _propagating.clear()
  }



  override def evaluate(head: Reactive[S]): Unit = {
    val dt = makeDynamicReevaluationTicket(indeps = head.state.incoming())
    val reevRes = head.reevaluate(dt, head.state.base(token))

    if (!dt.indepsChanged) {
      reevRes.forValue(writeValue(head))
      reevRes.forEffect(observe)
      if (reevRes.propagate) enqueueOutgoing(head)
    } else {
      val newLevel = maximumLevel(dt.indepsAfter) + 1
      val redo = head.state.level() < newLevel
      if (redo) {
        levelQueue.enqueue(newLevel)(head)
      } else {
        commitDependencyDiff(head, dt)
        reevRes.forValue(writeValue(head,newLevel))
        reevRes.forEffect(observe)
        if (reevRes.propagate) enqueueOutgoing(head, newLevel)
      }
    }



  }

  private def writeValue(head: ReSource[S], minLevel: Int = -42)(value: head.Value): Unit = {
    writeState(head)(value)
  }

  private def enqueueOutgoing(head: ReSource[S], minLevel: Int = -42) = {
    head.state.outgoing().foreach(levelQueue.enqueue(minLevel))
    _propagating += head
  }

  private def maximumLevel(dependencies: Set[ReSource[S]]): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.state.level()))

  override protected def ignite(reactive: Reactive[S], incoming: Set[ReSource[S]], ignitionRequiresReevaluation: Boolean): Unit = {
    val level = if (incoming.isEmpty) 0 else incoming.map(_.state.level()).max + 1
    reactive.state.updateLevel(level)

    incoming.foreach { dep =>
      dynamicDependencyInteraction(dep)
      discover(dep, reactive)
    }
    reactive.state.updateIncoming(incoming)

    if (ignitionRequiresReevaluation || incoming.exists(_propagating.contains)) {
      if (level <= levelQueue.currentLevel()) {
        evaluate(reactive)
      } else {
        levelQueue.enqueue(level)(reactive)
      }
    }
  }

  final override def initialize(ic: InitialChange[S]): Unit = {
      writeValue(ic.source)(ic.value)
      enqueueOutgoing(ic.source)
  }

  def propagationPhase(): Unit = levelQueue.evaluateQueue()
}
