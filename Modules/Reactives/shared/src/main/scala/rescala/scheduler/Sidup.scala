package rescala.scheduler

import rescala.core.{AdmissionTicket, InitialChange, Initializer, ReSource, ReevTicket, Scheduler}

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec

object SynchronizedSidup extends Sidup {
  val scheduler: Scheduler[State] = new TwoVersionScheduler[SidupTransaction] {
    override protected def makeTransaction(priorTx: Option[SidupTransaction]): SidupTransaction =
      new SidupTransaction

    override def schedulerName: String = "SidupSimple"

    override def forceNewTransaction[R](
        initialWrites: Set[ReSource.of[State]],
        admissionPhase: AdmissionTicket[State] => R
    ): R =
      synchronized {
        super.forceNewTransaction(initialWrites, admissionPhase)
      }
  }
}
trait Sidup extends Twoversion {

  type State[V] = SidupState[V]

  // currently use resource object directly
  type SourceId = Int
  val sidupCounter = new AtomicInteger(0)

  class SidupState[V](initialValue: V) extends TwoVersionState[V](initialValue) {
    var sources: Set[SourceId]  = Set(sidupCounter.getAndIncrement())
    var sourcesChanged: Boolean = false
    var activate: Boolean       = false
    var done: Boolean           = false

    def refreshSources(): Unit = {
      val oldSources = sources
      sources = incoming.flatMap(_.state.sources)
      sourcesChanged = oldSources != sources
    }
    override def updateIncoming(reactives: Set[ReSource]): Unit = {
      super.updateIncoming(reactives)
      refreshSources()
    }

    override def release(): Unit = {
      super.release()
      sourcesChanged = false
      activate = false
      done = false
    }
  }

  class SidupInitializer(currentTx: SidupTransaction) extends Initializer[State] {

    override protected[this] def makeDerivedStructState[V](initialValue: V): SidupState[V] =
      new SidupState(initialValue)
    override protected def initialize(
        reactive: Derived,
        incoming: Set[ReSource],
        needsReevaluation: Boolean
    ): Unit = {

      incoming.foreach { dep =>
        currentTx.discover(dep, reactive)
      }
      reactive.state.updateIncoming(incoming)
      // is set by the call above, but makes no sense for new reactives
      reactive.state.sourcesChanged = false

      if (needsReevaluation || incoming.exists(_.state.done)) {
        // somewhat strange workaround to force activation
        reactive.state.activate = true
        // immediate evaluation helps break dynamic creation cycle … sometimes
        if (currentTx.sources != null)
          currentTx.evaluateIn(reactive)(currentTx.makeDynamicReevaluationTicket(reactive.state.base(currentTx.token)))
        else currentTx.pokeLater(reactive)
      }
    }
  }

  class SidupTransaction extends TwoVersionTransactionImpl {

    var sources: Set[SourceId] = null

    override def initializationPhase(initialChanges: Map[ReSource, InitialChange[State]]): Unit = {
      val initsources = initialChanges.flatMap { case (s, ic) =>
        val isChange = ic.writeValue(ic.source.state.base(token), writeState(ic.source))
        if (isChange) {
          s.state.activate = true
          s.state.done = true
          schedule(s)
          Some(s)
        } else None
      }
      sources = initsources.flatMap(_.state.sources).toSet
      initsources.foreach(_.state.outgoing.foreach(pokeLater))
    }

    /** Store a single resettable ticket for the whole evaluation.
      * This optimization drastically reduces garbage generation of a relatively expensive object
      */
    private val reevaluationTicket: ReevTicket[State, _] = makeDynamicReevaluationTicket(null)

    private var evaluating: List[Derived]      = List.empty
    private var evaluatingLater: List[Derived] = List.empty

    def pokeLater(r: Derived): Unit = evaluating ::= r

    /** Overrides the evaluator, this is essentially an inlined callback */
    def evaluate(r: Derived): Unit = evaluateIn(r)(reevaluationTicket.reset(r.state.base(token)))
    def evaluateIn(reactive: Derived)(dt: ReevTicket[State, reactive.Value]): Unit = {
      if (!reactive.state.done) {
        val rinc = relevantIncoming(reactive)
        if (rinc.forall(_.state.done)) {
          // if the state of the reactive itself is activation, this means it has just been created and MUST be evaluated
          if (reactive.state.activate || rinc.exists(_.state.activate)) {
            val reevRes                             = reactive.reevaluate(dt)
            val dependencies: Option[Set[ReSource]] = reevRes.inputs()
            dependencies.foreach(commitDependencyDiff(reactive, reactive.state.incoming))
            // recompute relevant dependencies if there were dynamic changes
            val inc = dependencies.fold(rinc)(_ => relevantIncoming(reactive))
            if (inc.forall(_.state.done)) {
              if (inc.exists(_.state.sourcesChanged)) reactive.state.refreshSources()
              reevRes.forValue(writeState(reactive))
              reevRes.forEffect(observe)
              markDone(reactive, reevRes.activate)
            }
          } else {
            markDone(reactive, activate = false)
          }
        } else {
          evaluatingLater ::= reactive
        }
      }

    }

    private def markDone(reactive: Derived, activate: Boolean): Unit = {
      reactive.state.done = true
      schedule(reactive)
      reactive.state.activate = activate
      reactive.state.outgoing.foreach(pokeLater)
    }
    def relevantIncoming(head: Derived): Seq[ReSource] = {
      head.state.incoming.iterator.filter { r =>
        sources.exists(r.state.sources.contains)
      }.toSeq
    }

    override def beforeDynamicDependencyInteraction(dependency: ReSource): Unit = ()
    override def preparationPhase(initialWrites: Set[ReSource]): Unit           = ()
    @tailrec
    final override def propagationPhase(): Unit = {
      if (evaluating.nonEmpty) {
        val ev = evaluating
        evaluating = List.empty
        ev.foreach(evaluate)
        propagationPhase()
      } else if (evaluatingLater.nonEmpty) {
        evaluating = evaluatingLater
        evaluatingLater = List.empty
        propagationPhase()
      }
    }
    override def releasePhase(): Unit            = ()
    override def initializer: Initializer[State] = new SidupInitializer(this)
  }

}
