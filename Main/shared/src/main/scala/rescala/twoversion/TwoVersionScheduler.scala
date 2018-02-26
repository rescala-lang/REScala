package rescala.twoversion

import rescala.core.{Initializer, SchedulerImpl}

/**
  * Implementation of the turn handling defined in the Engine trait
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  * @tparam TImpl Turn type used by the engine
  */
trait TwoVersionScheduler[S <: TwoVersionStruct, TImpl <: TwoVersionPropagation[S] with Initializer[S]] extends SchedulerImpl[S, TImpl] {
  override private[rescala] def singleReadValueOnce[A](reactive: Signal[A]) = reactive.interpret(reactive.state.base(null))

  /** goes through the whole turn lifecycle
    * - create a new turn and put it on the stack
    * - run the lock phase
    *   - the turn knows which reactives will be affected and can do something before anything is really done
    * - run the admission phase
    *   - executes the user defined admission code
    * - run the propagation phase
    *   - calculate the actual new value of the reactive graph
    * - run the commit phase
    *   - do cleanups on the reactives, make values permanent and so on, the turn is still valid during this phase
    * - run the observer phase
    *   - run registered observers, the turn is no longer valid but the locks are still held.
    * - run the release phase
    *   - this must always run, even in the case that something above fails. it should do cleanup and free any locks to avoid starvation.
    * - run the party! phase
    *   - not yet implemented
    * */
  override private[rescala] def executeTurn[R](initialWrites: Traversable[ReSource], admissionPhase: (AdmissionTicket) => R) = {
    val turn = makeTurn(_currentTurn.value)

    val result = try {
      turn.preparationPhase(initialWrites)
      val result = withTurn(turn) {
        val admissionTicket = turn.makeAdmissionPhaseTicket()
        val admissionResult = admissionPhase(admissionTicket)
        turn.initializationPhase(admissionTicket.initialChanges)
        turn.propagationPhase()
        if (admissionTicket.wrapUp != null) admissionTicket.wrapUp(turn.makeWrapUpPhaseTicket())
        admissionResult
      }
      turn.commitPhase()
      result
    }
    catch {
      case e: Throwable =>
        turn.rollbackPhase()
        throw e
    }
    finally {
      turn.releasePhase()
    }
    turn.observerPhase()
    result
  }

  protected def makeTurn(priorTurn: Option[TImpl]): TImpl

}
