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

  private object _ObserverS extends ValuePersistency[Unit]((), isTransient = true, ignitionRequiresReevaluation = true)
  def SignalObserver[V]: ValuePersistency[Unit] = _ObserverS.asInstanceOf[ValuePersistency[Unit]]

  private object _ObserverE extends ValuePersistency[Unit]((), isTransient = true, ignitionRequiresReevaluation = false)
  def EventObserver[V]: ValuePersistency[Unit] = _ObserverE.asInstanceOf[ValuePersistency[Unit]]

  private object _ChangeEvent extends ValuePersistency[Pulse[Nothing]](Pulse.NoChange, isTransient = false, ignitionRequiresReevaluation = true)
  def ChangeEvent[V]: ValuePersistency[Pulse[V]] = _ChangeEvent.asInstanceOf[ValuePersistency[Pulse[V]]]

}
