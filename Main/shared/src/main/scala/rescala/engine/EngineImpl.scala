package rescala.engine

import rescala.graph.Struct

import scala.util.DynamicVariable

trait EngineImpl[S <: Struct, TTurn <: Turn[S]] extends Engine[S] {
  override type ExactTurn = TTurn
  override protected[rescala] def executeTurn[I, R](initialWrites: Traversable[Reactive], admissionPhase: AdmissionTicket => I, wrapUpPhase: (I, WrapUpTicket) => R): R = {
    // TODO: This should be broken up differently here, sort-of meeting in the middle with TwoVersionEngineImpl, something like:
    /*
      // scheduling performs framing/locking/whatever
      val turn = scheduling.prepareTurn(initialWrites)

      // admission phase is executed independently of scheduling/propagation algorithm.
      // the admission phase parameter probably shouldn't be the full-blown turn, but rather an "external" ticket
      // offering the pre-propagation external user API (now=before/admit etc.)
      val admissionResult = withTurn(turn) { Try { admissionPhase(turn) } }
      if(admissionResult.isFailure) initialWrites.foreach(_.unadmit(turn))

      // the (potentially decoupled) propagation algorithm issues reevaluations in a glitch-free order to scheduling.
      // Scheduling thus needs to offer an interface to propagation for submitting reevaluations, and possibly
      // a secondary method for submitting observer closures.
      // Not sure how to go about the reverse way, i.e., propagation needs to somewhere offer some interface to
      // scheduling for submitting retrofitting of reevaluations.
      // Upon scheduling receiving a ready reevaluation from propagation, scheduling then executes the reevaluation
      // using a dynamic/static ticket, which offers the user computation API (before/after etc.)
      propagation.propagateChanges(scheduling.propagationInterface(turn), initialWrites)
      // To support stuff like rollback upon failures during propagation, maybe scheduling can define a return type
      // for observer invocations that propagation channels through and outputs from this method? Some subtype of
      // Try[Unit] (for FullMV simply Success[Unit], for TwoVersion scheduling maybe Try[Set[Observers]]?).
      // The propagation result should then be combined as:
      // afterPropagation = admissionResult.flatMap(propagationResult)).

      // again the wrap-up turn parameter should not be the full-blown turn, but probably a second external ticket
      // offering a post-propagation external user API (before/now=after)
      val result = admissionResult.flatMap { i => withTurn(turn) { Try { wrapUpPhase(i, turn) } } }

      // scheduling cleans up locks/versions/whatever; this should probably also receive the propagationResult
      // (if implemented) to allow dispatch towards commit+observers or rollback for TwoVersion stuff?
      scheduling.cleanUpTurn(turn)

      result
    */

    val turn = makeTurn(initialWrites, currentTurn())
    executeInternal(turn, initialWrites, () => withTurn(turn){ admissionPhase(turn.makeAdmissionPhaseTicket()) }, { i: I => withTurn(turn){ wrapUpPhase(i, turn.makeWrapUpPhaseTicket()) } })
  }

  /**
    * Returns a new turn to be used by the engine
    *
    * @return New turn
    */
  protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[ExactTurn]): ExactTurn
  protected def executeInternal[I, R](turn: ExactTurn, initialWrites: Traversable[Reactive], admissionPhase: () => I, wrapUpPhase: I => R): R

  private val _currentTurn: DynamicVariable[Option[ExactTurn]] = new DynamicVariable[Option[ExactTurn]](None)
  override private[rescala] def currentTurn(): Option[ExactTurn] = _currentTurn.value
  // TODO currently the responsibility of setting the current turn around each reevaluation lies with each turn itself.
  // This is silly because this behavior is the same for every turn implementation. Moreover, turns can only set
  // the current turn for the available engine that they were started from. However, (probably because this TurnSource
  // design just isn't sound (yet?)) each reactive could have been created based on a different engine, just with
  // compatible structs and turns. In that case, the current turn for each reactive's respective engine should be set
  // during its reevaluation, which the reactive can only do itself. Thus, ideally, each reactive should call this
  // method and turns just should not be concerned with this responsibility at all.
  // TODO clean-up changes if we ever implement it this way:
  // - FullMV and ParallelLockSweep turns no longer need their engine reference.
  // - TwoVersionEngine no longer needs to wrap its propagationPhase
  private[rescala] def withTurn[R](turn: ExactTurn)(thunk: => R): R = _currentTurn.withValue(Some(turn))(thunk)
}

