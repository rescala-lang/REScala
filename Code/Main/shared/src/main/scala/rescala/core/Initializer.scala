package rescala.core

import rescala.core.Initializer.InitValues
import rescala.reactives.Signals.Diff

trait Initializer[S <: Struct] {
  /** Creates and correctly initializes new [[rescala.core.Derived]]s */
  final private[rescala] def create[V, T <: Derived[S]](incoming: Set[ReSource[S]],
                                                        initv: InitValues[V],
                                                        inite: Boolean,
                                                        creationTicket: CreationTicket[S])
                                                       (instantiateReactive: S#State[V, S] => T): T = {
    val state = makeDerivedStructState[V](initv)
    val reactive = instantiateReactive(state)
    register(reactive)
    ignite(reactive, incoming, inite)
    reactive
  }

  def accessTicket(): AccessTicket[S]


  protected[this] def register(reactive: ReSource[S]): Unit = ()

  /** Correctly initializes [[ReSource]]s */
  final private[rescala] def createSource[V, T <: ReSource[S]]
    (intv: InitValues[V], creationTicket: CreationTicket[S])(instantiateReactive: S#State[V, S] => T): T = {
    val state = makeSourceStructState[V](intv)
    val reactive = instantiateReactive(state)
    register(reactive)
    reactive
  }

  /** Creates the internal state of [[Derived]]s */
  protected[this] def makeDerivedStructState[V](valuePersistency: InitValues[V]): S#State[V, S]

  /** Creates the internal state of [[ReSource]]s */
  protected[this] def makeSourceStructState[V](valuePersistency: InitValues[V]): S#State[V, S] =
    makeDerivedStructState[V](valuePersistency)
  /**
    * to be implemented by the propagation algorithm, called when a new reactive has been instantiated and needs to be connected to the graph and potentially reevaluated.
    *
    * @param reactive                     the newly instantiated reactive
    * @param incoming                     a set of incoming dependencies
    * @param ignitionRequiresReevaluation true if the reactive must be reevaluated at creation even if none of its dependencies change in the creating turn.
    */
  protected[this] def ignite(reactive: Derived[S], incoming: Set[ReSource[S]], ignitionRequiresReevaluation: Boolean): Unit

}


object Initializer {

  trait Unchange[T] {
    def unchange(v: T): T
  }

  class ResetToNoChange[T] extends Unchange[Pulse[T]] {override def unchange(v: Pulse[T]): Pulse[T] = Pulse.NoChange}

  class KeepValue[T] extends Unchange[T] {override def unchange(v: T): T = v}
  class ResetCurrentKeepState[T] extends Unchange[(Pulse[T], Pulse[Diff[T]])] {
    override def unchange(v: (Pulse[T], Pulse[Diff[T]])): (Pulse[T], Pulse[Diff[T]]) = v.copy(_2 = Pulse.NoChange)
  }

  class InitValues[V](val initialValue: V, val unchange: Unchange[V])
  def Event[T] = new InitValues[Pulse[T]](Pulse.NoChange, new ResetToNoChange[T])
  def Change[T] = new InitValues[(Pulse[T], Pulse[Diff[T]])]((Pulse.NoChange, Pulse.NoChange), new ResetCurrentKeepState[T])
  def DerivedSignal[T] = new InitValues[Pulse[T]](Pulse.empty, new KeepValue[Pulse[T]])
  def Observer[T] = new InitValues(Pulse.NoChange, new ResetToNoChange[T])

  case class InitializedSignal[T](override val initialValue: T)
    extends InitValues[T](initialValue, new KeepValue[T]) {
  }
}
