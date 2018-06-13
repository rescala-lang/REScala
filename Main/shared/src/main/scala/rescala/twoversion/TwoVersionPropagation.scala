package rescala.twoversion

import rescala.core.{AdmissionTicket, InitialChange, ReSource, Struct, WrapUpTicket}

/**
  * Abstract propagation definition that defines phases for reactive propagation through dependent reactive elements.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait TwoVersionPropagation[S <: Struct]  {

  /**
    * Schedules a temporarily written change to be committed by the turn.
    *
    * @param committable Commitable element to be scheduled
    */
  def schedule(committable: Committable[S]): Unit

  /**
    * Locks (and potentially otherwise prepares) all affected reactive values to prevent interfering changes.
    *
    * @param initialWrites List of affected reactive values
    */
  def preparationPhase(initialWrites: Set[ReSource[S]]): Unit

  /**
    * Starts the propagation by applying the initial changes
    * @param initialChanges
    */
  def initializationPhase(initialChanges: Map[ReSource[S], InitialChange[S]]): Unit

  /**
    * Performs the actual propagation, setting the new (not yet committed) values for each reactive element.
    */
  def propagationPhase(): Unit

  /**
    * Commits all uncommitted changes to the reactive element.
    */
  def commitPhase(): Unit

  /**
    * Reverts all uncommitted changes to the reactive element.
    */
  def rollbackPhase(): Unit

  /**
    * Call all registered after-commit obverser functions.
    */
  def observerPhase(): Unit

  /**
    * Unlocks (and potentially otherwise reverts the propagation preparations for) each reactive value to allow future
    * turns to run on them.
    */
  def releasePhase(): Unit

  private[rescala] def makeAdmissionPhaseTicket(initialWrites: Set[ReSource[S]]): AdmissionTicket[S]
  private[rescala] def makeWrapUpPhaseTicket(): WrapUpTicket[S]


}
