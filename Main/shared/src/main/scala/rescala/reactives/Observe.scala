package rescala.reactives

import rescala.core._
import rescala.reactives.RExceptions.UnhandledFailureException

/**
  * Generic interface for observers that represent a function registered to trigger for every reevaluation of a reactive value.
  * Currently this interface is only used to allow a removal of registered observer functions.
  *
  * @tparam S Struct type used for the propagation of the signal
  */
trait Observe[S <: Struct] {
  def remove()(implicit fac: Scheduler[S]): Unit
}

object Observe {
  def dereferenceAllStrongObserversWARNINGonlyUseThisIfYouKnowWhatYouAreDoing(): Unit = strongObserveReferences.clear()

  private val strongObserveReferences = scala.collection.mutable.HashSet[Observe[_]]()

  private abstract class Obs[T, S <: Struct]
  (bud: S#State[Unit, S], dependency: ReSourciV[Pulse[T], S], fun: T => Unit, fail: Throwable => Unit, name: REName)
    extends Base[Unit, S](bud, name) with Reactive[S] with Observe[S]  {
    this: DisconnectableImpl[S] =>

    override protected[rescala] def reevaluate(turn: Turn[S], before: Unit, indeps: Set[ReSource[S]]): ReevaluationResult[Value, S] = {
      scheduleHandler(this, turn, dependency, fun, fail)
      ReevaluationResultWithoutValue[S](propagate = false)
    }
    override def remove()(implicit fac: Scheduler[S]): Unit = {
      disconnect()
      strongObserveReferences.synchronized(strongObserveReferences.remove(this))
    }
  }

  private def scheduleHandler[T, S <: Struct](obs: Obs[T,S], turn:Turn[S], dependency: ReSourciV[Pulse[T], S], fun: T => Unit, fail: Throwable => Unit) = {
    turn.makeStaticReevaluationTicket().staticDependPulse(dependency) match {
      case Pulse.NoChange =>
      case Pulse.empty =>
      case Pulse.Value(v) => turn.observe(() => fun(v))
      case Pulse.Exceptional(t) =>
        if (fail eq null) throw new UnhandledFailureException(obs, t)
        else turn.observe(() => fail(t))
    }
  }

  def weak[T, S <: Struct](dependency: ReSourciV[Pulse[T], S], fireImmediately: Boolean = true)(fun: T => Unit, fail: Throwable => Unit)(implicit ct: CreationTicket[S]): Observe[S] = {
    ct(initTurn => initTurn.create[Unit, Obs[T, S]](Set(dependency), if(fireImmediately) ValuePersistency.SignalObserver else ValuePersistency.EventObserver) { state =>
      new Obs[T, S](state, dependency, fun, fail, ct.rename) with DisconnectableImpl[S]
    })
  }

  def strong[T, S <: Struct](dependency: ReSourciV[Pulse[T], S], fireImmediately: Boolean = true)(fun: T => Unit, fail: Throwable => Unit)(implicit maybe: CreationTicket[S]): Observe[S] = {
    val obs = weak(dependency, fireImmediately)(fun, fail)
    strongObserveReferences.synchronized(strongObserveReferences.add(obs))
    obs
  }

}
