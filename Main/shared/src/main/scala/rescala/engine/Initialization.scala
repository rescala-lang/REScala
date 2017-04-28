package rescala.engine

import rescala.graph._


trait InitializationImpl[S <: Struct] extends Turn[S] {
  final private[rescala] def create[P, T <: Reactive[S]](incoming: Set[Reactive[S]], valuePersistency: ValuePersistency[P])(instantiateReactive: S#State[P, S] => T): T = {
    val state = makeStructState(valuePersistency)
    val reactive = instantiateReactive(state)
    ignite(reactive, incoming, valuePersistency)
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
  protected def ignite(reactive: Reactive[S], incoming: Set[Reactive[S]], valuePersistency: ValuePersistency[_]): Unit
}

sealed class ValuePersistency[+V](
  val initialValue: V,
  val isTransient: Boolean,
  val ignitionRequiresReevaluation: Boolean,
  val dynamic: Boolean
)

object ValuePersistency {
  object Event extends ValuePersistency[Pulse[Nothing]](Pulse.NoChange, isTransient = true, ignitionRequiresReevaluation = true, dynamic = false)
  object DynamicEvent extends ValuePersistency[Pulse[Nothing]](Pulse.NoChange, isTransient = true, ignitionRequiresReevaluation = true, dynamic = true)
  object Signal extends ValuePersistency[Change[Nothing]](Pulse.empty, isTransient = false, ignitionRequiresReevaluation = true, dynamic = false)
  object DynamicSignal extends ValuePersistency[Change[Nothing]](Pulse.empty, isTransient = false, ignitionRequiresReevaluation = true, dynamic = true)
  case class InitializedSignal[V](override val initialValue: Change[V])
    extends ValuePersistency[Change[V]](initialValue, isTransient = false, ignitionRequiresReevaluation = false, dynamic = false)
}

