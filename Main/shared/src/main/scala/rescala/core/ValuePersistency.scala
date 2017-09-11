package rescala.core

sealed class ValuePersistency[V](
  val initialValue: V,
  val isTransient: Boolean,
  val ignitionRequiresReevaluation: Boolean
)

object ValuePersistency {
  // Events do not have a value, so reevaluating them at ignition is pointless in theory.
  // DynamicEvents however are part badly implemented and part not well-enough understood,
  // so they DO require an initial reevaluation to establish their initial dependencies correctly.
  // Since superfluous reevaluation of events without a good reason however do not actually have
  // any effect, we simply set all events to just be reevaluated upon ignition unconditionally,
  // which ensures that dynamic events work correctly and doesn't hurt others.
  private object _Event extends ValuePersistency[Pulse[Nothing]](Pulse.NoChange, isTransient = true, ignitionRequiresReevaluation = true)
  def Event[V]: ValuePersistency[Pulse[V]] = _Event.asInstanceOf[ValuePersistency[Pulse[V]]]
  private object _DerivedSignal extends ValuePersistency[Pulse.Change[Nothing]](Pulse.empty, isTransient = false, ignitionRequiresReevaluation = true)
  def DerivedSignal[V]: ValuePersistency[Pulse[V]] = _DerivedSignal.asInstanceOf[ValuePersistency[Pulse[V]]]

  case class InitializedSignal[V: ReSerializable](override val initialValue: Pulse[V])
    extends ValuePersistency[Pulse[V]](initialValue, isTransient = false, ignitionRequiresReevaluation = false) {
    def serializable: ReSerializable[Pulse[V]] = ReSerializable.pulseSerializable
  }
}
