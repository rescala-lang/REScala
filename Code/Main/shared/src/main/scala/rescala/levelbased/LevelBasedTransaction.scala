package rescala.levelbased

import rescala.core.{InitialChange, ReSource, Derived, ReevTicket}
import rescala.levelbased.LevelQueue.noLevelIncrease
import rescala.twoversion.TwoVersionTransactionImpl

import scala.collection.mutable.ArrayBuffer

/** Further implementation of level-based propagation based on the common propagation implementation.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait LevelBasedTransaction[S <: LevelStruct] extends TwoVersionTransactionImpl[S] with LevelQueue.Evaluator[S] {

  /** Stores all active reactives in case we create more later and need to reevaluate them. */
  private val _propagating: ArrayBuffer[ReSource[S]] = ArrayBuffer[ReSource[S]]()

  val levelQueue = new LevelQueue[S](this)

  /** Store a single resettable ticket for the whole evaluation.
    * This optimization drastically reduces garbage generation of a relatively expensive object
    */
  private val reevaluationTicket: ReevTicket[_, S] = makeDynamicReevaluationTicket(null)

  /** Overrides [[LevelQueue.Evaluator]], this is essentially an inlined callback */
  override def evaluate(r: Derived[S]): Unit = evaluateIn(r)(reevaluationTicket.reset(r.state.base(token)))
  def evaluateIn(head: Derived[S])(dt: ReevTicket[head.Value, S]): Unit = {
    val reevRes = head.reevaluate(dt)

    val dependencies: Option[Set[ReSource[S]]] = reevRes.dependencies()
    val minimalLevel                           = dependencies.fold(0)(nextLevel)
    val redo                                   = head.state.level() < minimalLevel
    if (redo) {
      levelQueue.enqueue(minimalLevel)(head)
    } else {
      dependencies.foreach(commitDependencyDiff(head, head.state.incoming))
      reevRes.forValue(writeState(head))
      reevRes.forEffect(observe)
      if (reevRes.propagate) enqueueOutgoing(head, minimalLevel)
    }
  }

  private def enqueueOutgoing(head: ReSource[S], minLevel: Int): ArrayBuffer[ReSource[S]] = {
    head.state.outgoing().foreach(levelQueue.enqueue(minLevel))
    _propagating += head
  }

  private def nextLevel(dependencies: Set[ReSource[S]]): Int =
    if (dependencies.isEmpty) 0 else dependencies.map(_.state.level()).max + 1

  override protected def ignite(
      reactive: Derived[S],
      incoming: Set[ReSource[S]],
      ignitionRequiresReevaluation: Boolean
  ): Unit = {
    val level = nextLevel(incoming)
    reactive.state.updateLevel(level)

    incoming.foreach { dep =>
      beforeDynamicDependencyInteraction(dep)
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

  final override def prepareInitialChange(ic: InitialChange[S]): Unit = {
    val n = ic.writeValue(ic.source.state.base(token), writeState(ic.source))
    if (n) enqueueOutgoing(ic.source, noLevelIncrease)
  }

  def propagationPhase(): Unit = levelQueue.evaluateQueue()
}
