package rescala.reactives

import rescala.engine.TurnSource
import rescala.graph.Pulse.NoChange
import rescala.graph._
import rescala.propagation.{Turn, ReevaluationTicket}

object Events {


  private class StaticEvent[T, S <: Struct](_bud: S#Type[T, Reactive[S]], expr: Turn[S] => Pulse[T], override val toString: String)
    extends Base[T, S](_bud) with Event[T, S] with StaticReevaluation[S] with Disconnectable[S] {
    override def calculatePulse()(implicit turn: Turn[S]): Pulse[T] = Pulse.tryCatch(expr(turn), onEmpty = NoChange)
  }

  private class DynamicEvent[T, S <: Struct](_bud: S#Type[T, Reactive[S]], expr: ReevaluationTicket[S] => Pulse[T]) extends Base[T, S](_bud) with Event[T, S] with DynamicReevaluation[S] with Disconnectable[S]  {
    override def calculatePulseDependencies(ticket: ReevaluationTicket[S]): Pulse[T] = {
      Pulse.tryCatch(expr(ticket), onEmpty = NoChange)
    }
  }

  /** the basic method to create static events */
  def static[T, S <: Struct](name: String, dependencies: Reactive[S]*)(calculate: Turn[S] => Pulse[T])(implicit ticket: TurnSource[S]): Event[T, S] = ticket { initTurn =>
    val dependencySet: Set[Reactive[S]] = dependencies.toSet
    initTurn.create(dependencySet) {
      new StaticEvent[T, S](initTurn.makeStructState(initialIncoming = dependencySet, transient = true), calculate, name)
    }
  }

  /** create dynamic events */
  def dynamic[T, S <: Struct](dependencies: Reactive[S]*)(expr: ReevaluationTicket[S] => Option[T])(implicit ticket: TurnSource[S]): Event[T, S] = {
    ticket { initialTurn =>
      initialTurn.create(dependencies.toSet, dynamic = true)(
        new DynamicEvent[T, S](initialTurn.makeStructState(transient = true), expr.andThen(Pulse.fromOption)))
    }
  }

}
