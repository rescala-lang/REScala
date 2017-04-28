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
    * @param incoming a set of incoming dependencies
    * @param dynamic false if the set of incoming dependencies is the correct final set of dependencies (static reactive)
    *                true if the set of incoming dependencies is just a best guess for the initial dependencies.
    * @param valuePersistency the value persistency
    * @param instantiateReactive The factory method to instantiate the reactive with the newly created state.
    * @tparam P Reactive value type
    * @tparam T Reactive subtype of the reactive element
    * @return Connected reactive element
    */
  private[rescala] def create[P, T <: Reactive[S]](incoming: Set[Reactive[S]], dynamic: Boolean, valuePersistency: ValuePersistency[P])(instantiateReactive: S#State[P, S] => T): T = {
    val state = makeStructState(valuePersistency)
    val reactive = instantiateReactive(state)
    ignite(reactive, incoming, dynamic, valuePersistency)
    reactive
  }

  /**
    * to be implemented by the scheduler, called when a new state storage object is required for instantiating a new reactive.
    * @param valuePersistency the value persistency
    * @tparam P the stored value type
    * @return the initialized state storage
    */
  protected def makeStructState[P](valuePersistency: ValuePersistency[P]): S#State[P, S]

  /**
    * to be implemented by the propagation algorithm, called when a new reactive has been instantiated and needs to be connected to the graph and potentially reevaluated.
    * @param reactive the newly instantiated reactive
    * @param incoming a set of incoming dependencies
    * @param dynamic false if the set of incoming dependencies is the correct final set of dependencies (static reactive)
    *                true if the set of incoming dependencies is just a best guess for the initial dependencies.
    */
  protected def ignite(reactive: Reactive[S], incoming: Set[Reactive[S]], dynamic: Boolean, valuePersistency: ValuePersistency[_]): Unit

  /**
    * Registers a new handler function that is called after all changes were written and committed.
    *
    * @param f Handler function to register.
    */
  def observe(f: () => Unit): Unit

}

sealed trait ValuePersistency[+V] {
  val initialValue: V
  val isTransient: Boolean
  val ignitionRequiresReevaluation: Boolean
}
case object Transient extends ValuePersistency[Pulse[Nothing]] {
  override val initialValue: Pulse[Nothing] = Pulse.NoChange
  override val isTransient: Boolean = true
  override val ignitionRequiresReevaluation: Boolean = false
}
sealed trait Steady[+V] extends ValuePersistency[V] {
  override val isTransient: Boolean = false
}
case object Derived extends Steady[Change[Nothing]] {
  override val initialValue: Change[Nothing] = Pulse.empty
  override val ignitionRequiresReevaluation: Boolean = true
}
case class Accumulating[V](override val initialValue: Change[V]) extends Steady[Change[V]] {
  override val ignitionRequiresReevaluation: Boolean = false
}
