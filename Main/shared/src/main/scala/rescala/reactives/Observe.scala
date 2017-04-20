package rescala.reactives

import rescala.engine.{Engine, TurnSource}
import rescala.graph.{Base, Disconnectable, Pulse, Pulsing, Reactive, ReevaluationResult, Struct}
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

  private val strongObserveReferences = scala.collection.mutable.HashMap[Observe[_], Boolean]()

  private abstract class Obs[T, S <: Struct](bud: S#State[Pulse[T], S], dependency: Pulsing[Pulse[T], S], fun: T => Unit, fail: Throwable => Unit) extends Base[T, S](bud) with Reactive[S] with Observe[S]  {
    this: Disconnectable[S] =>

    override protected[rescala] def reevaluate(ticket: S#Ticket[S]): ReevaluationResult[Value, S] = {
      scheduleHandler(this, ticket, dependency, fun, fail)
      ReevaluationResult.Static(Pulse.NoChange)
    }
    override def remove()(implicit fac: Engine[S, Turn[S]]): Unit = {
      disconnect()
      strongObserveReferences.synchronized(strongObserveReferences.remove(this))
    }
  }

  private def scheduleHandler[T, S <: Struct](obs: Obs[T,S], ticket:S#Ticket[S], dependency: Pulsing[Pulse[T], S], fun: T => Unit, fail: Throwable => Unit) = {
    val turn = ticket.turn()
    dependency.pulse(ticket) match {
      case Pulse.NoChange =>
      case Pulse.empty =>
      case Pulse.Change(v) => turn.observe(() => fun(v))
      case Pulse.Exceptional(t) =>
        if (fail eq null) throw new UnhandledFailureException(obs, t)
        else turn.observe(() => fail(t))
    }
  }

  def weak[T, S <: Struct](dependency: Pulsing[Pulse[T], S])(fun: T => Unit, fail: Throwable => Unit)(implicit maybe: TurnSource[S]): Observe[S] = {
    val incoming = Set[Reactive[S]](dependency)
    maybe(initTurn => initTurn.create(incoming) {
      val obs = new Obs(initTurn.makeStructState[Pulse[T]](Pulse.NoChange, initialIncoming = incoming, transient = false), dependency, fun, fail) with Disconnectable[S]
      val ticket = initTurn.makeTicket
      scheduleHandler(obs, ticket, dependency, fun, fail)
      obs
    })
  }

  def strong[T, S <: Struct](dependency: Pulsing[Pulse[T], S])(fun: T => Unit, fail: Throwable => Unit)(implicit maybe: TurnSource[S]): Observe[S] = {
    val obs = weak(dependency)(fun, fail)
    strongObserveReferences.synchronized(strongObserveReferences.put(obs, true))
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
  this : Pulsing[Pulse[P], S] =>
  /** add an observer */
  final def observe(
    onSuccess: P => Unit,
    onFailure: Throwable => Unit = null
  )(implicit ticket: TurnSource[S]): Observe[S] = Observe.strong(this)(onSuccess, onFailure)
}
