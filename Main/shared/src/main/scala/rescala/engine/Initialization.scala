package rescala.engine

import rescala.graph._


trait Creation[S <: Struct] {
  private[rescala] def create[P, T <: Reactive[S]](incoming: Set[Reactive[S]], valuePersistency: ValuePersistency[P])(instantiateReactive: S#State[P, S] => T): T
}

trait CreationImpl[S <: Struct] extends Creation[S] {
  /**
    * Connects a reactive element with potentially existing dependencies and prepares re-evaluations to be
    * propagated based on the turn's propagation scheme
    *
    * @param incoming a set of incoming dependencies
    * @param valuePersistency the value persistency
    * @param instantiateReactive The factory method to instantiate the reactive with the newly created state.
    * @tparam P Reactive value type
    * @tparam R Reactive subtype of the reactive element
    * @return Connected reactive element
    */
  final private[rescala] def create[P, T <: Reactive[S]](incoming: Set[Reactive[S]], valuePersistency: ValuePersistency[P])(instantiateReactive: S#State[P, S] => T): T = {
    val state = makeStructState(valuePersistency)
    val reactive = instantiateReactive(state)
    ignite(reactive, incoming, valuePersistency.ignitionRequiresReevaluation)
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
    * @param ignitionRequiresReevaluation true if the reactive must be reevaluated at creation even if none of its dependencies change in the creating turn.
    */
  protected def ignite(reactive: Reactive[S], incoming: Set[Reactive[S]], ignitionRequiresReevaluation: Boolean): Unit
}

sealed class ValuePersistency[V](
  val initialValue: V,
  val isTransient: Boolean,
  val ignitionRequiresReevaluation: Boolean
)

object ValuePersistency {
  // Events do not have a value, so reevaluating them at ignition is pointless in theory.
  // DynamicEvents however are part badly implemented and part not well-enough unterstood,
  // so they DO require an initial reevaluation to establish their initial dependencies correctly.
  // Since superfluous reevaluation of events without a good reason however do not actually have
  // any effect, we simply set all events to just be reevaluated upon ignition unconditionally,
  // which ensures that dynamic events work correctly and doesn't hurt others.
  private object _Event extends ValuePersistency[Pulse[Nothing]](Pulse.NoChange, isTransient = true, ignitionRequiresReevaluation = true)
  def Event[V]: ValuePersistency[V] = _Event.asInstanceOf[ValuePersistency[V]]
  private object _DerivedSignal extends ValuePersistency[Change[Nothing]](Pulse.empty, isTransient = false, ignitionRequiresReevaluation = true)
  def DerivedSignal[V]: ValuePersistency[V] = _DerivedSignal.asInstanceOf[ValuePersistency[V]]

  case class InitializedSignal[V: ReSerializable](override val initialValue: Change[V])
    extends ValuePersistency[Pulse[V]](initialValue, isTransient = false, ignitionRequiresReevaluation = false) {
    def serializable: ReSerializable[Pulse[V]] = implicitly
  }
}

