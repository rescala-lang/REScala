package reactives.scheduler

import reactives.core
import reactives.core.{
  AccessHandler, AdmissionTicket, InitialChange, Observation, ReSource, ReadAs, ReevTicket, SchedulerWithDynamicScope,
  Tracing, Transaction
}

import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

class Token

trait Twoversion {

  type State[V] <: TwoVersionState[V]
  type ReSource = reactives.core.ReSource.of[State]
  type Derived  = reactives.core.Derived.of[State]

  /** State that implements both the buffered pulse and the buffering capabilities itself. */
  abstract class TwoVersionState[V](protected[reactives] var current: V) {

    private var owner: Token = null
    private var update: V    = scala.compiletime.uninitialized

    def write(value: V, token: Token): Unit = {
      assert(owner == null, s"buffer owned by $owner written by $token")
      update = value
      owner = token
    }
    def base(token: Token): V = current
    def get(token: Token): V  = { if (token eq owner) update else current }

    def commit(r: V => V): Unit = {
      if (update != null) current = r(update)
      release()
    }
    def release(): Unit = {
      update = null.asInstanceOf[V]
      owner = null
    }

    /* incoming and outgoing changes */

    private var _incoming: Set[ReSource]  = Set.empty
    protected var _outgoing: Set[Derived] = Set.empty

    def updateIncoming(reactives: Set[ReSource]): Unit = _incoming = reactives
    def outgoing: Iterable[Derived]                    = _outgoing
    def incoming: Set[ReSource]                        = _incoming
    def discoveredBy(reactive: Derived): Unit          = _outgoing += reactive
    def droppedBy(reactive: Derived): Unit             = _outgoing -= reactive
  }

  /** Implementation of the turn handling defined in the Engine trait
    *
    * @tparam Tx Transaction type used by the scheduler
    */
  trait TwoVersionScheduler[Tx <: TwoVersionTransaction]
      extends SchedulerWithDynamicScope[State, Tx] {
    private[reactives] def singleReadValueOnce[A](reactive: ReadAs.of[State, A]): A =
      reactive.read(reactive.state.base(null))

    /** goes through the whole turn lifecycle
      * - create a new turn and put it on the stack
      * - run the lock phase
      *   - the turn knows which reactives will be affected and can do something before anything is really done
      *     - run the admission phase
      *   - executes the user defined admission code
      *     - run the propagation phase
      *   - calculate the actual new value of the reactive graph
      *     - run the commit phase
      *   - do cleanups on the reactives, make values permanent and so on, the turn is still valid during this phase
      *     - run the observer phase
      *   - run registered observers, the turn is no longer valid but the locks are still held.
      *     - run the release phase
      *   - this must always run, even in the case that something above fails. it should do cleanup and free any locks to avoid starvation.
      *     - run the party! phase
      *   - not yet implemented
      */
    override def forceNewTransaction[R](
        initialWrites: Set[ReSource.of[State]],
        admissionPhase: AdmissionTicket[State] => R
    ): R = {
      val tx = makeTransaction(dynamicScope.maybeTransaction)

      val txhash                    = tx.hashCode()
      def tracePhase(phase: String) = Tracing.observe(Tracing.Transaction(txhash, phase))
      tracePhase("started")

      val result =
        try {
          tracePhase("preparation")
          tx.preparationPhase(initialWrites)
          val result = dynamicScope.withDynamicInitializer(tx) {
            tracePhase("admission")
            val admissionTicket = tx.makeAdmissionPhaseTicket(initialWrites)
            val admissionResult = admissionPhase(admissionTicket)
            tx.initializationPhase(admissionTicket.initialChanges)
            tracePhase("propagation")
            tx.propagationPhase()
            if (admissionTicket.wrapUp != null)
              tracePhase("wrapUp")
              admissionTicket.wrapUp(tx)
            admissionResult
          }
          tracePhase("commit")
          tx.commitPhase()
          result
        } catch {
          case e: Throwable =>
            tracePhase("rollback")
            tx.rollbackPhase()
            throw e
        } finally {
          tracePhase("release")
          tx.releasePhase()
        }
      tracePhase("observer")
      tx.observerPhase()
      tracePhase("ended")
      result
    }

    protected def makeTransaction(priorTx: Option[Tx]): Tx

  }

  /** Abstract propagation definition that defines phases for reactive propagation through dependent reactive elements. */
  sealed trait TwoVersionTransaction extends Transaction[Twoversion.this.State] {

    /** Schedules a temporarily written change to be committed by the turn. */
    def schedule(committable: ReSource): Unit

    /** Locks (and potentially otherwise prepares) all affected reactive values to prevent interfering changes.
      *
      * @param initialWrites List of affected reactive values
      */
    def preparationPhase(initialWrites: Set[ReSource]): Unit

    /** Starts the propagation by applying the initial changes */
    def initializationPhase(initialChanges: Map[ReSource, InitialChange[State]]): Unit

    /** Performs the actual propagation, setting the new (not yet committed) values for each reactive element. */
    def propagationPhase(): Unit

    /** Commits all uncommitted changes to the reactive element. */
    def commitPhase(): Unit

    /** Reverts all uncommitted changes to the reactive element. */
    def rollbackPhase(): Unit

    /** Call all registered after-commit obverser functions. */
    def observerPhase(): Unit

    /** Unlocks (and potentially otherwise reverts the propagation preparations for) each reactive value to allow future
      * turns to run on them.
      */
    def releasePhase(): Unit

    private[reactives] def makeAdmissionPhaseTicket(initialWrites: Set[ReSource]): AdmissionTicket[State]

  }

  /** Basic implementation of the most fundamental propagation steps as defined by AbstractPropagation.
    * Only compatible with spore definitions that store a pulse value and support graph operations.
    */
  trait TwoVersionTransactionImpl extends TwoVersionTransaction {

    val token: Token = new Token()

    val toCommit      = ListBuffer[ReSource]()
    val observers     = ListBuffer[Observation]()
    val followups     = ListBuffer[Observation]()
    var commitStarted = false

    override def schedule(commitable: ReSource): Unit = { toCommit += commitable; () }

    def observe(f: Observation): Unit = {
      if (commitStarted) throw new IllegalStateException(
        s"Added observation to transaction (${this}), but it is too late in its lifecycle. " +
        s"This may happen due to capturing a transaction reference such that it survives outside of its dynamic scope."
      )
      observers += f
      ()
    }

    override def followup(obs: Observation): Unit = {
      if (commitStarted) throw new IllegalStateException(
        s"Added observation to transaction (${this}), but it is too late in its lifecycle. " +
        s"This may happen due to capturing a transaction reference such that it survives outside of its dynamic scope."
      )
      followups += obs
      ()
    }

    override def commitPhase(): Unit = {
      commitStarted = true
      toCommit.foreach { r => r.state.commit(r.commit) }
    }

    override def rollbackPhase(): Unit = toCommit.foreach(r => r.state.release())

    override def observerPhase(): Unit = {
      var failure: Throwable = null
      observers.foreach { n =>
        try n.execute()
        catch { case NonFatal(e) => failure = e }
      }
      // find some failure and rethrow the contained exception
      // we should probably aggregate all of the exceptions,
      // but this is not the place to invent exception aggregation
      if (failure != null) throw failure

      followups.foreach { n =>
        try n.execute()
        catch { case NonFatal(e) => failure = e }
      }
      if (failure != null) throw failure
    }

    final def commitDependencyDiff(node: Derived, current: Set[ReSource])(updated: Set[ReSource]): Unit = {
      val indepsRemoved = current -- updated
      val indepsAdded   = updated -- current
      indepsRemoved.foreach(drop(_, node))
      indepsAdded.foreach(discover(_, node))
      node.state.updateIncoming(updated)
    }

    private[reactives] override def discover(source: ReSource, sink: Derived): Unit = {
      super.discover(source, sink)
      source.state.discoveredBy(sink)
    }
    private[reactives] override def drop(source: ReSource, sink: Derived): Unit = {
      super.drop(source, sink)
      source.state.droppedBy(sink)
    }

    /** allow the propagation to handle dynamic access to reactives */
    def beforeDynamicDependencyInteraction(dependency: ReSource): Unit

    object accessHandler extends AccessHandler[State] {
      override def dynamicAccess(reactive: ReSource): reactive.Value =
        TwoVersionTransactionImpl.this.dynamicAfter(reactive)
      override def staticAccess(reactive: ReSource): reactive.Value = reactive.state.get(token)
    }

    override private[reactives] def makeAdmissionPhaseTicket(initialWrites: Set[ReSource]): AdmissionTicket[State] =
      new AdmissionTicket[State](this, initialWrites)
    private[reactives] def makeDynamicReevaluationTicket[V, N](b: V): ReevTicket[State, V] =
      new ReevTicket[State, V](this, b, accessHandler)

    override def access(reactive: ReSource): reactive.Value =
      TwoVersionTransactionImpl.this.dynamicAfter(reactive)

    private[reactives] def dynamicAfter[P](reactive: ReSource): reactive.Value = {
      // Note: This only synchronizes reactive to be serializable-synchronized, but not glitch-free synchronized.
      // Dynamic reads thus may return glitched values, which the reevaluation handling implemented in subclasses
      // must account for by repeating glitched reevaluations!
      beforeDynamicDependencyInteraction(reactive)
      reactive.state.get(token)
    }
    def writeState(pulsing: ReSource)(value: pulsing.Value): Unit = {
      pulsing.state.write(value, token)
      Tracing.observe(Tracing.Value(pulsing, Tracing.ValueWrapper(value)))
      this.schedule(pulsing)
    }

  }
}
