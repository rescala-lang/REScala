package rescala.core

import rescala.core.Initializer.InitValues

trait Initializer[S <: Struct] {
  /** Creates and correctly initializes new [[Reactive]]s */
  final private[rescala] def create[P, T <: Reactive[S], N](incoming: Set[ReSource[S]],
                                                            initv: InitValues[P, N],
                                                            inite: Boolean)
                                                           (instantiateReactive: S#State[P, S, N] => T): T = {
    val state = makeDerivedStructState[P, N](initv)
    val reactive = instantiateReactive(state)
    ignite(reactive, incoming, inite)
    reactive
  }

  /** Correctly initializes [[ReSource]]s */
  final private[rescala] def createSource[V, T <: ReSource[S], N]
    (intv: InitValues[V, N])(instantiateReactive: S#State[V, S, N] => T): T = {
    val state = makeSourceStructState[V, N](intv)
    instantiateReactive(state)
  }

  /** Creates the internal state of [[Reactive]]s */
  protected[this] def makeDerivedStructState[V, N](valuePersistency: InitValues[V, N]): S#State[V, S, N]

  /**  Creates the internal state of [[ReSource]]s */
  protected[this] def makeSourceStructState[V, N](valuePersistency: InitValues[V, N]): S#State[V, S, N] =
    makeDerivedStructState[V, N](valuePersistency)
  /**
    * to be implemented by the propagation algorithm, called when a new reactive has been instantiated and needs to be connected to the graph and potentially reevaluated.
    * @param reactive the newly instantiated reactive
    * @param incoming a set of incoming dependencies
    * @param ignitionRequiresReevaluation true if the reactive must be reevaluated at creation even if none of its dependencies change in the creating turn.
    */
  protected[this] def ignite(reactive: Reactive[S], incoming: Set[ReSource[S]], ignitionRequiresReevaluation: Boolean): Unit
}


object Initializer {

  class InitValues[+V, +N](val initialValue: V, val noNotification: N)
  val Event = new InitValues((), Pulse.NoChange)
  val Change = new InitValues(Pulse.NoChange, Pulse.NoChange)
  val DerivedSignal = new InitValues(Pulse.empty, ())
  val Observer = new InitValues((), ())

  case class InitializedSignal[V: ReSerializable](override val initialValue: V)
    extends InitValues[V, Unit](initialValue, ()) {
    def serializable: ReSerializable[Pulse[V]] = ReSerializable.pulseSerializable
  }
}
