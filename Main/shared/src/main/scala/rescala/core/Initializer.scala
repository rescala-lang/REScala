package rescala.core

import rescala.core.Initializer.Param

trait Initializer[S <: Struct] {
  /** Creates and correctly initializes new [[Reactive]]s */
  final private[rescala] def create[P, T <: Reactive[S], N]
  (incoming: Set[ReSource[S]], valuePersistency: Param[P])(instantiateReactive: S#State[P, S, N] => T): T = {
    val state = makeDerivedStructState[P, N](valuePersistency)
    val reactive = instantiateReactive(state)
    ignite(reactive, incoming, valuePersistency.ignitionRequiresReevaluation)
    reactive
  }

  /** Correctly initializes [[ReSource]]s */
  final private[rescala] def createSource[P, T <: ReSource[S], N]
  (valuePersistency: Param[P])(instantiateReactive: S#State[P, S, N] => T): T = {
    val state = makeSourceStructState[P, N](valuePersistency)
    instantiateReactive(state)
  }

  /** Creates the internal state of [[Reactive]]s */
  protected[this] def makeDerivedStructState[P, N](valuePersistency: Param[P]): S#State[P, S, N]

  /**  Creates the internal state of [[Interp]]s */
  protected[this] def makeSourceStructState[P, N](valuePersistency: Param[P]): S#State[P, S, N] =
    makeDerivedStructState[P, N](valuePersistency)
  /**
    * to be implemented by the propagation algorithm, called when a new reactive has been instantiated and needs to be connected to the graph and potentially reevaluated.
    * @param reactive the newly instantiated reactive
    * @param incoming a set of incoming dependencies
    * @param ignitionRequiresReevaluation true if the reactive must be reevaluated at creation even if none of its dependencies change in the creating turn.
    */
  protected[this] def ignite(reactive: Reactive[S], incoming: Set[ReSource[S]], ignitionRequiresReevaluation: Boolean): Unit
}


object Initializer {

  sealed class Param[V](
    val initialValue: V,
    val isTransient: Boolean,
    val ignitionRequiresReevaluation: Boolean
  )

  private object _Event extends Param[Pulse[Nothing]](Pulse.NoChange, isTransient = true, ignitionRequiresReevaluation = false)
  def Event[V]: Param[Pulse[V]] = _Event.asInstanceOf[Param[Pulse[V]]]

  private object _DynamicEvent extends Param[Pulse[Nothing]](Pulse.NoChange, isTransient = true, ignitionRequiresReevaluation = true)
  def DynamicEvent[V]: Param[Pulse[V]] = _DynamicEvent.asInstanceOf[Param[Pulse[V]]]

  private object _DerivedSignal extends Param[Pulse.Change[Nothing]](Pulse.empty, isTransient = false, ignitionRequiresReevaluation = true)
  def DerivedSignal[V]: Param[Pulse[V]] = _DerivedSignal.asInstanceOf[Param[Pulse[V]]]

  case class InitializedSignal[V: ReSerializable](override val initialValue: Pulse[V])
    extends Param[Pulse[V]](initialValue, isTransient = false, ignitionRequiresReevaluation = false) {
    def serializable: ReSerializable[Pulse[V]] = ReSerializable.pulseSerializable
  }

  private object _ObserverS extends Param[Unit]((), isTransient = true, ignitionRequiresReevaluation = true)
  def SignalObserver[V]: Param[Unit] = _ObserverS.asInstanceOf[Param[Unit]]

  private object _ObserverE extends Param[Unit]((), isTransient = true, ignitionRequiresReevaluation = false)
  def EventObserver[V]: Param[Unit] = _ObserverE.asInstanceOf[Param[Unit]]

  private object _ChangeEvent extends Param[Pulse[Nothing]](Pulse.NoChange, isTransient = false, ignitionRequiresReevaluation = true)
  def ChangeEvent[V]: Param[Pulse[V]] = _ChangeEvent.asInstanceOf[Param[Pulse[V]]]

}
