package rescala.core

sealed class ValuePersistency[V](
  val initialValue: V,
  val isTransient: Boolean,
  val ignitionRequiresReevaluation: Boolean
)

object ValuePersistency {
  private object _Event extends ValuePersistency[Pulse[Nothing]](Pulse.NoChange, isTransient = true, ignitionRequiresReevaluation = false)
  def Event[V]: ValuePersistency[Pulse[V]] = _Event.asInstanceOf[ValuePersistency[Pulse[V]]]

  private object _DynamicEvent extends ValuePersistency[Pulse[Nothing]](Pulse.NoChange, isTransient = true, ignitionRequiresReevaluation = true)
  def DynamicEvent[V]: ValuePersistency[Pulse[V]] = _DynamicEvent.asInstanceOf[ValuePersistency[Pulse[V]]]

  private object _DerivedSignal extends ValuePersistency[Pulse.Change[Nothing]](Pulse.empty, isTransient = false, ignitionRequiresReevaluation = true)
  def DerivedSignal[V]: ValuePersistency[Pulse[V]] = _DerivedSignal.asInstanceOf[ValuePersistency[Pulse[V]]]

  case class InitializedSignal[V: ReSerializable](override val initialValue: Pulse[V])
    extends ValuePersistency[Pulse[V]](initialValue, isTransient = false, ignitionRequiresReevaluation = false) {
    def serializable: ReSerializable[Pulse[V]] = ReSerializable.pulseSerializable
  }
}
