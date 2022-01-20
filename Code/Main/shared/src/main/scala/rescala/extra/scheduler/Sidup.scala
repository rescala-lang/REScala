package rescala.extra.scheduler

import rescala.scheduler.Twoversion

trait Sidup extends Twoversion {

  type State[V] = SidupState[V]

  // currently use resource object directly
  type SourceId = ReSource

  class SidupState[V](initialValue: V) extends TwoVersionState[V](initialValue) {
    var sources: Set[SourceId]  = Set.empty[SourceId]
    var sourcesChanged: Boolean = false
    var activate: Boolean      = false
    var done: Boolean          = false

    def refreshSources(): Boolean = {
      val oldSources = sources
      sources = incoming.flatMap(_.state.sources)
      sourcesChanged = oldSources != sources
      sourcesChanged
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

  class SidupInitializer(currentTx: SidupTransaction) extends Initializer {
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

      if (needsReevaluation || reactive.state.sources.exists(currentTx.sources.contains)) {
        currentTx.pokeLater(reactive)
      }
    }
  }

  class SidupTransaction extends TwoVersionTransactionImpl {
    var sources: Set[SourceId] = null

    override def initializationPhase(initialChanges: Map[ReSource, InitialChange]): Unit = {
      sources = initialChanges.flatMap { case (s, ic) =>
        val isChange = ic.writeValue(ic.source.state.base(token), writeState(ic.source))
        if (isChange) {
          s.state.activate = true
          s.state.done = true
          schedule(s)
          Some(s)
        } else None
      }.toSet
      sources.foreach(_.state.outgoing.foreach(pokeLater))
    }

    /** Store a single resettable ticket for the whole evaluation.
      * This optimization drastically reduces garbage generation of a relatively expensive object
      */
    private val reevaluationTicket: ReevTicket[_] = makeDynamicReevaluationTicket(null)

    private var evaluating: List[Derived] = List.empty
    private var evaluatingLater : List[Derived] = List.empty

    def pokeLater(r: Derived): Unit = evaluating ::= r

    /** Overrides the evaluator, this is essentially an inlined callback */
    def evaluate(r: Derived): Unit = evaluateIn(r)(reevaluationTicket.reset(r.state.base(token)))
    def evaluateIn(head: Derived)(dt: ReevTicket[head.Value]): Unit = {
      val reevRes = head.reevaluate(dt)
      val dependencies: Option[Set[ReSource]] = reevRes.inputs()
      dependencies.foreach(commitDependencyDiff(head, head.state.incoming))
      val inc = relevantIncoming(head)
      if (inc.forall(_.state.done)) {
        if (inc.exists(_.state.sourcesChanged)) head.state.refreshSources()
        reevRes.forValue(writeState(head))
        schedule(head)
        reevRes.forEffect(observe)
        head.state.activate = reevRes.activate
        if (head.state.activate) head.state.outgoing.foreach(pokeLater)
      }
    }
    private def relevantIncoming(head: Derived): Seq[ReSource] = {
      head.state.incoming.iterator.filter(_.state.sources.exists(sources.contains)).toSeq
    }

    override def beforeDynamicDependencyInteraction(dependency: ReSource): Unit = ()
    override def preparationPhase(initialWrites: Set[ReSource]): Unit           = ()
    override def propagationPhase(): Unit                                       = {
      while (evaluating.nonEmpty) {
        val ev = evaluating
        evaluating = List.empty
        ev.foreach { r =>
          if (relevantIncoming(r).forall(_.state.done)) evaluate(r)
          else evaluatingLater ::= r
        }
      }
      if (evaluatingLater.nonEmpty) {
        evaluating = evaluatingLater
        evaluatingLater = List.empty
        propagationPhase()
      }
    }
    override def releasePhase(): Unit                                           = ()
    override def initializer: Initializer                                       = new SidupInitializer(this)
  }


}
