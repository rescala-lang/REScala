package rescala.scheduler

import rescala.core.{AdmissionTicket, InitialChange, Initializer, ReSource, ReevTicket, Scheduler}

import java.util.PriorityQueue
import scala.collection.mutable.ListBuffer

object LevelbasedVariants extends Levelbased {
  type State[V] = LevelState[V]

  private[rescala] class SimpleNoLock extends LevelBasedTransaction {
    override protected def makeDerivedStructState[V](initialValue: V): State[V] = new LevelState(initialValue)

    override def releasePhase(): Unit = ()

    override def preparationPhase(initialWrites: Set[ReSource.of[State]]): Unit = {}

    override def beforeDynamicDependencyInteraction(dependency: ReSource): Unit = {}
  }

  val unmanaged: Scheduler[State] =
    new TwoVersionScheduler[SimpleNoLock] {
      override protected def makeTransaction(priorTx: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock()

      override def schedulerName: String = "Unmanaged"
    }

  val synchron: Scheduler[State] = new TwoVersionScheduler[SimpleNoLock] {
    override protected def makeTransaction(priorTx: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock

    override def schedulerName: String = "Synchron"

    override def forceNewTransaction[R](
        initialWrites: Set[ReSource.of[State]],
        admissionPhase: AdmissionTicket[State] => R
    ): R =
      synchronized {
        super.forceNewTransaction(initialWrites, admissionPhase)
      }
  }
}

trait Levelbased extends Twoversion {

  type State[V] <: LevelState[V]

  class LevelState[V](initialValue: V) extends TwoVersionState[V](initialValue) {

    private var _level: Int = 0

    def level(): Int = _level

    def updateLevel(i: Int): Int = {
      val max = math.max(i, _level)
      _level = max
      max
    }
  }

  /** Further implementation of level-based propagation based on the common propagation implementation. */
  trait LevelBasedTransaction extends TwoVersionTransactionImpl with LevelQueue.Evaluator with Initializer[State] {

    /** Stores all active reactives in case we create more later and need to reevaluate them. */
    private val _propagating: ListBuffer[ReSource] = ListBuffer[ReSource]()

    lazy val levelQueue = new LevelQueue(this)

    /** Store a single resettable ticket for the whole evaluation.
      * This optimization drastically reduces garbage generation of a relatively expensive object
      */
    private val reevaluationTicket: ReevTicket[State, _] = makeDynamicReevaluationTicket(null)

    /** Overrides the evaluator, this is essentially an inlined callback */
    override def evaluate(r: Derived): Unit = evaluateIn(r)(reevaluationTicket.reset(r.state.base(token)))
    def evaluateIn(head: Derived)(dt: ReevTicket[head.State, head.Value]): Unit = {
      val reevRes = head.reevaluate(dt)

      val dependencies: Option[Set[ReSource]] = reevRes.inputs()
      val minimalLevel                        = dependencies.fold(0)(nextLevel)
      val redo                                = head.state.level() < minimalLevel
      if (redo) {
        levelQueue.enqueue(minimalLevel)(head)
      } else {
        dependencies.foreach(commitDependencyDiff(head, head.state.incoming))
        reevRes.forValue(writeState(head))
        reevRes.forEffect(observe)
        if (reevRes.activate) enqueueOutgoing(head, minimalLevel)
      }
    }

    private def enqueueOutgoing(head: ReSource, minLevel: Int): Unit = {
      head.state.outgoing.foreach(levelQueue.enqueue(minLevel))
      _propagating += head
      ()
    }

    private def nextLevel(dependencies: Set[ReSource]): Int =
      if (dependencies.isEmpty) 0 else dependencies.map(_.state.level()).max + 1

    override def initializer: Initializer[State] = this

    override protected def initialize(
        reactive: Derived,
        incoming: Set[ReSource],
        needsReevaluation: Boolean
    ): Unit = {
      val level = nextLevel(incoming)
      reactive.state.updateLevel(level)

      incoming.foreach { dep =>
        beforeDynamicDependencyInteraction(dep)
        discover(dep, reactive)
      }
      reactive.state.updateIncoming(incoming)

      if (needsReevaluation || incoming.exists(_propagating.contains)) {
        if (level <= levelQueue.currentLevel()) {
          evaluateIn(reactive)(makeDynamicReevaluationTicket(reactive.state.base(token)))
        } else {
          levelQueue.enqueue(level)(reactive)
        }
      }
    }

    final override def initializationPhase(initialChanges: Map[ReSource, InitialChange[State]]): Unit =
      initialChanges.values.foreach(prepareInitialChange)

    final def prepareInitialChange(ic: InitialChange[State]): Unit = {
      val n = ic.writeValue(ic.source.state.base(token), writeState(ic.source))
      if (n) enqueueOutgoing(ic.source, LevelQueue.noLevelIncrease)
    }

    def propagationPhase(): Unit = levelQueue.evaluateQueue()
  }

  /** Level-based queue used the determine an evaluation order for reactive elements
    *
    * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
    */
  final private[Levelbased] class LevelQueue(evaluator: LevelQueue.Evaluator) {

    import LevelQueue._

    private val elements = new PriorityQueue[QueueElement]()

    /** Gets the level of the current head element of the queue (if existing).
      * Used to determine if newly created reactives have to be evaluated.
      *
      * @return Level of the current queue head
      */
    def currentLevel(): Int =
      if (elements.peek() == null) Int.MaxValue
      else elements.peek().level

    /** Adds a new reactive element to the queue
      *
      * @param minLevel      Minimum level to assign the the element (overrides the elements original level if larger)
      * @param needsEvaluate Indicates if the element needs re-evaluation itself, otherwise it is just a level change
      * @param dep           Element to add to the queue
      */
    def enqueue(minLevel: Int, needsEvaluate: Boolean = true)(dep: Derived): Unit = {
      elements.offer(QueueElement(dep.state.level(), dep, minLevel, needsEvaluate))
      ()
    }

    /** Handles a queue element by applying the given evaluator to it and scheduling the next elements for evaluation
      *
      * @param queueElement Element to evaluate
      */
    private def handleElement(queueElement: QueueElement): Unit = {
      val QueueElement(headLevel, head, headMinLevel, reevaluate) = queueElement
      // handle level increases
      if (headLevel < headMinLevel) {
        head.state.updateLevel(headMinLevel)
        enqueue(headMinLevel, reevaluate)(head)
        head.state.outgoing.foreach { r =>
          if (r.state.level() <= headMinLevel)
            enqueue(headMinLevel + 1, needsEvaluate = false)(r)
        }
      } else if (reevaluate) {
        evaluator.evaluate(head)
      }
    }

    /** Evaluates all currently queued elements by applying the given evaluator to them. */
    def evaluateQueue(): Unit = {
      var current = elements.poll()
      var next    = elements.peek()
      while (current != null) {
        // if the current and next reactive are equal, merge the queue entries
        if (next != null && current.reactive == next.reactive) {
          next.minLevel = next.minLevel max current.minLevel
          next.needsEvaluate ||= current.needsEvaluate
        } else {
          handleElement(current)
        }
        current = elements.poll()
        next = elements.peek()
      }

    }

    /** Removes a reactive element from the queue
      *
      * @param reactive Element to remove from the queue
      */
    def remove(reactive: Derived): Unit = {
      val it = elements.iterator()
      while (it.hasNext) if (it.next().reactive eq reactive) it.remove()
    }
  }

  private[Levelbased] object LevelQueue {

    trait Evaluator {
      def evaluate(r: Derived): Unit
    }

    /** The value to not increase the level of an enqueued [[QueueElement]]. */
    def noLevelIncrease: Int = Int.MinValue

    private case class QueueElement(
        level: Int,
        reactive: Derived,
        var minLevel: Int,
        var needsEvaluate: Boolean
    ) extends Comparable[QueueElement] {
      // order by level, then by reactive
      val order: Long = (level.toLong << 32) | (reactive.hashCode.toLong & 0x00000000ffffffffL)
      override def compareTo(o: QueueElement): Int = java.lang.Long.compare(order, o.order)
    }
  }
}
