package rescala.core

import rescala.core.Initializer.InitValues
import rescala.reactives.Signals.Diff

trait Initializer[S <: Struct] {
  /** Creates and correctly initializes new [[Reactive]]s */
  final private[rescala] def create[V, T <: Reactive[S]](incoming: Set[ReSource[S]],
                                                         initv: InitValues[V],
                                                         inite: Boolean)
                                                        (instantiateReactive: S#State[V, S] => T): T = {
    val state = makeDerivedStructState[V](initv)
    val reactive = instantiateReactive(state)
    ignite(reactive, incoming, inite)
    reactive
  }

  /** Correctly initializes [[ReSource]]s */
  final private[rescala] def createSource[V, T <: ReSource[S]]
    (intv: InitValues[V])(instantiateReactive: S#State[V, S] => T): T = {
    val state = makeSourceStructState[V](intv)
    instantiateReactive(state)
  }

  /** Creates the internal state of [[Reactive]]s */
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
  protected[this] def ignite(reactive: Reactive[S], incoming: Set[ReSource[S]], ignitionRequiresReevaluation: Boolean): Unit
}


object Initializer {

  trait Unchange[T] {
    def unchange(v: T): T
  }

  class EUnchange[T] extends Unchange[Pulse[T]] {override def unchange(v: Pulse[T]): Pulse[T] = Pulse.NoChange}

  class SUnchange[T] extends Unchange[T] {override def unchange(v: T): T = v}
  class CUnchange[T] extends Unchange[(Pulse[T], Pulse[Diff[T]])] {
    override def unchange(v: (Pulse[T], Pulse[Diff[T]])): (Pulse[T], Pulse[Diff[T]]) = v.copy(_2 = Pulse.NoChange)
  }

  class InitValues[V](val initialValue: V, val unchange: Unchange[V])
  def Event[T] = new InitValues[Pulse[T]](Pulse.NoChange, new EUnchange[T])
  def Change[T] = new InitValues[(Pulse[T], Pulse[Diff[T]])]((Pulse.NoChange, Pulse.NoChange), new CUnchange[T])
  def DerivedSignal[T] = new InitValues[Pulse[T]](Pulse.empty, new SUnchange[Pulse[T]])
  def Observer[T] = new InitValues(Pulse.NoChange, new EUnchange[T])

  case class InitializedSignal[T: ReSerializable](override val initialValue: T)
    extends InitValues[T](initialValue, new SUnchange[T]) {
    def serializable: ReSerializable[T] = implicitly
  }
}
