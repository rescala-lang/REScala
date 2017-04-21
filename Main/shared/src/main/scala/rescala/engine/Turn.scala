package rescala.engine

import rescala.graph.{DynamicTicket, Pulsing, Reactive, StaticTicket, Struct}

/**
  * The Turn interface glues the reactive interface and the propagation implementation together.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Turn[S <: Struct] {
  private[rescala] def makeStructState[P](initialValue: P, transient: Boolean = true, initialIncoming: Set[Reactive[S]] = Set.empty[Reactive[S]], hasState: Boolean = false): S#State[P, S]


  def dynamic(): DynamicTicket[S] = new DynamicTicket[S](this)
  def static(): StaticTicket[S] = new StaticTicket[S](this)

  /**
    * Synchronize for access (i.e., [[before()]] or [[after()]]) on this node when
    * synchronization is unknown. Multiple invocations are redundant, but not harmful outside of an
    * implementation-dependent performance penalty.
    *
    * @param reactive the reactive to be dynamically accessed
    */
  private[rescala] def dynamicDependencyInteraction(reactive: Reactive[S]): Unit

  /**
    * Read value from before this turn. Only call this if you know that you are synchronized with this node:
    * Reads on dependencies where an edge exists (e.g., reading a static dependency) is always synchronized.
    * Reads on other nodes must be synchronized through [[dynamicDependencyInteraction()]] first.
    * @param pulsing the node to be read
    * @tparam P the node's storage type
    * @return the stored value from before this turn
    */
  private[rescala] def before[P](pulsing: Pulsing[P, S]): P
  /**
    * Read value from after this turn. Implementations may return the node's current value, including
    * changes already made by this turn but disregarding potential future changes, or may suspend to
    * return only the value that is final until the end of this turn.
    * Only call this if you know that you are synchronized with this node:
    * Reads on dependencies where an edge exists (e.g., reading a static dependency) is always synchronized.
    * Reads on other nodes must be synchronized through [[dynamicDependencyInteraction()]] first.
    * @param pulsing the node to be read
    * @tparam P the node's storage type
    * @return the stored value from after this turn
    */
  private[rescala] def after[P](pulsing: Pulsing[P, S]): P

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
