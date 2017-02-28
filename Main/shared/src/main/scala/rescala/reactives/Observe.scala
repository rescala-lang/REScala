package rescala.reactives

import java.util.concurrent.ConcurrentHashMap

import rescala.engine.{Engine, TurnSource}
import rescala.graph._
import rescala.propagation.Turn
import rescala.reactives.RExceptions.UnhandledFailureException

/**
  * Generic interface for observers that represent a function registered to trigger for every reevaluation of a reactive value.
  * Currently this interface is only used to allow a removal of registered observer functions.
  *
  * @tparam S Struct type used for the propagation of the signal
  */
trait Observe[S <: Struct] {
  def remove()(implicit fac: Engine[S, Turn[S]]): Unit
}

object Observe {

  private val strongObserveReferences = new ConcurrentHashMap[Observe[_], Boolean]()

  private class Obs[T, S <: Struct](bud: S#StructType[T, Reactive[S]], dependency: Pulsing[T, S], fun: T => Unit, fail: Throwable => Unit) extends Base[T, S](bud) with Reactive[S] with Observe[S] with Disconnectable[S] {
    override protected[rescala] def computeReevaluationResult()(implicit turn: Turn[S]): ReevaluationResult[S] = {
      scheduleHandler(this, turn, dependency, fun, fail)
      ReevaluationResult.Static(changed = false)
    }
    override def remove()(implicit fac: Engine[S, Turn[S]]): Unit = {
      disconnect()
      strongObserveReferences.remove(this: Observe[_])
    }
  }

  private def scheduleHandler[T, S <: Struct](obs: Obs[T,S], turn: Turn[S], dependency: Pulsing[T, S], fun: T => Unit, fail: Throwable => Unit) = {
    dependency.pulse(turn) match {
      case Pulse.NoChange =>
      case Pulse.empty =>
      case Pulse.Change(v) => turn.observe(() => fun(v))
      case Pulse.Exceptional(t) =>
        if (fail eq null) throw new UnhandledFailureException(obs, t)
        else turn.observe(() => fail(t))
    }
  }

  def weak[T, S <: Struct](dependency: Pulsing[T, S])(fun: T => Unit, fail: Throwable => Unit = null)(implicit maybe: TurnSource[S]): Observe[S] = {
    val incoming = Set[Reactive[S]](dependency)
    maybe(initTurn => initTurn.create(incoming) {
      val obs = new Obs(initTurn.makeStructState[T, Reactive[S]](initialIncoming = incoming, transient = false), dependency, fun, fail)
      scheduleHandler(obs, initTurn, dependency, fun, fail)
      obs
    })
  }

  def strong[T, S <: Struct](dependency: Pulsing[T, S])(fun: T => Unit, fail: Throwable => Unit = null)(implicit maybe: TurnSource[S]): Observe[S] = {
    val obs = weak(dependency)(fun)
    strongObserveReferences.put(obs, true)
    obs
  }

}

/**
  * Reactives that can be observed by a function outside the reactive graph
  *
  * @tparam P Value type stored by the pulse of the reactive value
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Observable[+P, S <: Struct] {
  this : Pulsing[P, S] =>
  /** add an observer */
  final def observe(
    onSuccess: P => Unit,
    onFailure: Throwable => Unit = null
  )(implicit ticket: TurnSource[S]): Observe[S] = Observe.strong(this)(onSuccess, onFailure)
}
