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

  /**  Creates the internal state of [[ReSource]]s */
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

  sealed trait Param[V] {
    def initialValue: V
    val isTransient: Boolean
    val ignitionRequiresReevaluation: Boolean
  }

  val Event: Param[Nothing] = new Param[Nothing]{
    override def initialValue: Nothing = ???
    override val isTransient: Boolean = true
    override val ignitionRequiresReevaluation: Boolean = false
  }

  object DynamicEvent extends Param[Nothing]{
    override def initialValue: Nothing = ???
    override val isTransient: Boolean = true
    override val ignitionRequiresReevaluation: Boolean = true
  }

  private object _DerivedSignal extends Param[Pulse.Change[Nothing]]{
    override val initialValue: Pulse.Change[Nothing] = Pulse.empty
    override val isTransient: Boolean = false
    override val ignitionRequiresReevaluation: Boolean = true
  }
  def DerivedSignal[V]: Param[Pulse[V]] = _DerivedSignal.asInstanceOf[Param[Pulse[V]]]

  case class InitializedSignal[V: ReSerializable](override val initialValue: Pulse[V])
    extends Param[Pulse[V]] {
    override val isTransient: Boolean = false
    override val ignitionRequiresReevaluation: Boolean = false
    def serializable: ReSerializable[Pulse[V]] = ReSerializable.pulseSerializable
  }

  private object _ObserverS extends Param[Unit] {
    override val initialValue: Unit = ()
    override val isTransient: Boolean = true
    override val ignitionRequiresReevaluation: Boolean = true
  }
  def SignalObserver[V]: Param[Unit] = _ObserverS.asInstanceOf[Param[Unit]]

  private object _ObserverE extends Param[Unit] {
    override val initialValue: Unit = ()
    override val isTransient: Boolean = true
    override val ignitionRequiresReevaluation: Boolean = false
  }
  def EventObserver[V]: Param[Unit] = _ObserverE.asInstanceOf[Param[Unit]]

  private object _ChangeEvent extends Param[Pulse[Nothing]] {
    override val initialValue: Pulse[Nothing] = Pulse.NoChange
    override val isTransient: Boolean = false
    override val ignitionRequiresReevaluation: Boolean = true
  }
  def ChangeEvent[V]: Param[Pulse[V]] = _ChangeEvent.asInstanceOf[Param[Pulse[V]]]

}
