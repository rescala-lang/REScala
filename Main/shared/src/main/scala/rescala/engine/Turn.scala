package rescala.engine

import rescala.graph._

/**
  * The Turn interface glues the reactive interface and the propagation implementation together.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Turn[S <: Struct] {
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
    * @param incomingOrDynamic the static set of incoming dependencies or [[None]] if dynamic reevaluation
    * @param valueOrTransient The initial value for creating the state, or [[None]] if transient
    * @param hasAccumulatingState true if the reactive's value is accumulated over time,
    *                             false if it is a pure transformation of its parameters.
    * @param instantiateReactive The factory method to instantiate the reactive with the newly created state.
    * @tparam P Reactive value type
    * @tparam T Reactive subtype of the reactive element
    * @return Connected reactive element
    */
  private[rescala] def create[P, T <: Reactive[S]](incomingOrDynamic: Option[Set[Reactive[S]]], valueOrTransient: Option[Change[P]], hasAccumulatingState: Boolean)(instantiateReactive: S#State[Pulse[P], S] => T): T = {
    val state = makeStructState(valueOrTransient, hasAccumulatingState)
    val reactive = instantiateReactive(state)
    ignite(reactive, incomingOrDynamic)
    reactive
  }

  /**
    * to be implemented by the scheduler, called when a new state storage object is required for instantiating a new reactive.
    * @param valueOrTransient the initial value in case the reactive has a steady value, or [[None]] if it's transient
    * @param hasAccumulatingState true if the reactive's value is accumulated over time,
    *                             false if it is a pure transformation of its parameters.
    * @tparam P the stored value type
    * @return the initialized state storage
    */
  protected def makeStructState[P](valueOrTransient: Option[Change[P]], hasAccumulatingState: Boolean): S#State[Pulse[P], S]

  /**
    * to be implemented by the propagation algorithm, called when a new reactive has been instantiated and needs to be connected to the graph and potentially reevaluated.
    * @param reactive the newly instantiated reactive
    * @param incomingOrDynamic the static set of incoming dependencies or [[None]] if dynamic reevaluation
    * @tparam T
    */
  protected def ignite[T <: Reactive[S]](reactive: T, incomingOrDynamic: Option[Set[Reactive[S]]]): Unit

  /**
    * Registers a new handler function that is called after all changes were written and committed.
    *
    * @param f Handler function to register.
    */
  def observe(f: () => Unit): Unit

}
