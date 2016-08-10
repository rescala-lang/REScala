package rescala.propagation

import rescala.graph.{Buffer, Pulse, Reactive, Struct}

/**
  * The Turn interface glues the reactive interface and the propagation implementation together.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Turn[S <: Struct] {
  /**
    * Reads the pulse of the given spore.
    *
    * @param budP Spore to read the pulse from
    * @tparam P Stored pulse value type
    * @return Buffer containing the stored pulse of the spore
    */
  def pulses[P](budP: S#SporeP[P, _]): Buffer[Pulse[P]]

  /**
    * Replaces the incoming dependencies for a given spore. Used for dynamic reactive evaluation.
    *
    * @param bud Spore to replace dependencies for
    * @param newDependencies New dependencies to assign to a spore
    * @tparam R Reactive value type of the incoming dependencies of the spore
    */
  def updateIncoming[R](bud: S#Spore[R], newDependencies: Set[R]): Unit

  /**
    * Reads the incoming dependencies of a given spore.
    *
    * @param bud Spore to read the dependencies from
    * @tparam R Reactive value type of the incoming dependencies of the spore
    * @return Incoming dependencies of the spore
    */
  def incoming[R](bud: S#Spore[R]): Set[R]

  /**
    * Creates a new spore initialized with the given parameters
    *
    * @param initialValue Initially stored pulse of the spore
    * @param transient Indicates if the spore is transient (meaning that updates to it's pulse are reverted when committing)
    * @param initialIncoming Initial incoming dependencies of the spore
    * @tparam P Stored pulse value type
    * @tparam R Reactive value type of the incoming dependencies of the spore
    * @return
    */
  def bud[P, R](initialValue: Pulse[P] = Pulse.NoChange, transient: Boolean = true, initialIncoming: Set[R] = Set.empty[R]): S#SporeP[P, R]

  /**
    * Called to allow turn to handle dynamic access to reactive elements
    *
    * @param reactive Reactive element to handle
    */
  def dependencyInteraction(reactive: Reactive[S]): Unit

  /**
    * Connects a reactive element with potentially existing dependencies and prepares re-evaluations to be
    * propagated based on the turn's propagation scheme
    *
    * @param dependencies Existing reactive elements the element depends on
    * @param dynamic Indicates if the element uses dynamic re-evaluation to determine it's dependencies
    * @param f Reactive element to prepare and connect
    * @tparam T Reactive subtype of the reactive element
    * @return Connected reactive element
    */
  def create[T <: Reactive[S]](dependencies: Set[Reactive[S]], dynamic: Boolean = false)(f: => T): T

  /**
    * Schedules a temporarily written change to be committed by the turn.
    *
    * @param committable Commitable element to be scheduled
    */
  def schedule(committable: Committable): Unit

  /**
    * Registers a new handler function that is called after all changes were written and committed.
    *
    * @param f Handler function to register.
    */
  def observe(f: => Unit): Unit

  /**
    * Calls a given function and collects all dependencies dynamically marked as used during its evaluation.
    *
    * @param f Function to execute and collect dependencies for
    * @tparam T Return type of the function
    * @return Return value of the function and set of all reactive values marked as its dependencies
    */
  def collectDependencies[T](f: => T): (T, Set[Reactive[S]])

  /**
    * Marks a reactive element as used as dynamic dependency, so it is returned by `collectDependencies`
    *
    * @param dependency Reactive element to mark
    */
  def useDependency(dependency: Reactive[S]): Unit
}
