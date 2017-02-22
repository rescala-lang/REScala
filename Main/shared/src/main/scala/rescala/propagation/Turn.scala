package rescala.propagation

import rescala.graph.{Pulse, PulseOption, Reactive, Stateful, Struct}
import rescala.twoversion.Committable

/**
  * The Turn interface glues the reactive interface and the propagation implementation together.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Turn[S <: Struct] {

  /**
    * Creates a new spore initialized with the given parameters
    *
    * @param initialValue    Initially stored pulse of the spore
    * @param transient       Indicates if the spore is transient (meaning that updates to it's pulse are reverted when committing)
    * @param initialIncoming Initial incoming dependencies of the spore
    * @tparam P Stored pulse value type
    * @tparam R Reactive value type of the incoming dependencies of the spore
    * @return
    */
  private[rescala] def makeStructState[P, R](initialValue: Pulse[P] = Pulse.NoChange, transient: Boolean = true, initialIncoming: Set[R] = Set.empty[R]): S#StructType[P, R]

  /**
    * Called to allow turn to handle dynamic access to reactive elements
    *
    * @param reactive Reactive element to handle
    */
  private[rescala] def dynamicDependencyInteraction(reactive: Reactive[S]): Unit

  /**
    * Connects a reactive element with potentially existing dependencies and prepares re-evaluations to be
    * propagated based on the turn's propagation scheme
    *
    * @param dependencies Existing reactive elements the element depends on
    * @param dynamic      Indicates if the element uses dynamic re-evaluation to determine it's dependencies
    * @param f            Reactive element to prepare and connect
    * @tparam T Reactive subtype of the reactive element
    * @return Connected reactive element
    */
  private[rescala] def create[T <: Reactive[S]](dependencies: Set[Reactive[S]], dynamic: Boolean = false)(f: => T): T

  /**
    * Schedules a temporarily written change to be committed by the turn.
    *
    * @param committable Commitable element to be scheduled
    */
  // TODO should not use twoversion.Committable here
  def schedule(committable: Committable): Unit

  /**
    * Registers a new handler function that is called after all changes were written and committed.
    *
    * @param f Handler function to register.
    */
  def observe(f: => Unit): Unit

}
