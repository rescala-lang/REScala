package rescala.propagation

import rescala.graph.{Reactive, Struct}

/**
  * The Turn interface glues the reactive interface and the propagation implementation together.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Turn[S <: Struct] {
  private[rescala] def makeStructState[P](initialValue: P, transient: Boolean = true, initialIncoming: Set[Reactive[S]] = Set.empty[Reactive[S]], hasState: Boolean = false): S#State[P, S]


  def makeTicket(): S#Ticket[S]

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
    * Registers a new handler function that is called after all changes were written and committed.
    *
    * @param f Handler function to register.
    */
  def observe(f: () => Unit): Unit

}
