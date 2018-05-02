package rescala.levelbased

import rescala.core.{InitialChange, ReSource, Reactive, ReevTicket}
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

  val reevaluationTicket: ReevTicket[_, S] = makeDynamicReevaluationTicket(null)

  override def evaluate(r: Reactive[S]): Unit = evaluateIn(r)(reevaluationTicket.reset(r.state.base(token)))
  def evaluateIn(head: Reactive[S])(dt: ReevTicket[head.Value, S]): Unit = {
    val reevRes = head.reevaluate(dt)

    val dependencies: Option[Set[ReSource[S]]] = reevRes.getDependencies()
    val minimalLevel = dependencies.fold(0)(maximumLevel(_) + 1)
    val redo = head.state.level() < minimalLevel
    if (redo) {
      levelQueue.enqueue(minimalLevel)(head)
    } else {
      dependencies.foreach(commitDependencyDiff(head, head.state.incoming()))
      reevRes.forValue(writeState(head))
      reevRes.forEffect(observe)
      if (reevRes.propagate) enqueueOutgoing(head, minimalLevel)
    }
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
        evaluateIn(reactive)(makeDynamicReevaluationTicket(reactive.state.base(token)))
      } else {
        levelQueue.enqueue(level)(reactive)
      }
    }
  }

  final override def initialize(ic: InitialChange[S]): Unit = {
    val n = ic.writeValue(ic.source.state.base(token), writeState(ic.source))
    if (n) enqueueOutgoing(ic.source)
  }

  def propagationPhase(): Unit = levelQueue.evaluateQueue()
}
