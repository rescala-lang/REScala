package rescala.reactives

import rescala.engine.TurnSource
import rescala.graph.Pulse.NoChange
import rescala.graph.{Base, Disconnectable, Pulse, Reactive, ReevaluationResult, Struct}
import rescala.propagation.{DynamicTicket, StaticTicket}
import rescala.reactives.Signals.Diff

object Events {

  private abstract class StaticEvent[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: StaticTicket[S] => Pulse[T], override val toString: String)
    extends Base[T, S](_bud) with Event[T, S] {
    override protected[rescala] def reevaluate(ticket: S#Ticket[S]): ReevaluationResult[Value, S] = ReevaluationResult.Static(Pulse.tryCatch(expr(ticket.static()), onEmpty = NoChange))
  }

  private abstract class ChangeEvent[T, S <: Struct](_bud: S#State[Pulse[Diff[T]], S], signal: Signal[T, S])
    extends Base[Diff[T], S](_bud) with Event[Diff[T], S] {
    override protected[rescala] def reevaluate(ticket: S#Ticket[S]): ReevaluationResult[Value, S] = {
      val pulse = {
        val from = signal.stable(ticket)
        val to = signal.pulse(ticket)
        if (from != to) Pulse.Change(Diff(from, to))
        else Pulse.NoChange
      }
      ReevaluationResult.Static(pulse)
    }
  }

  private abstract class DynamicEvent[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: DynamicTicket[S] => Pulse[T]) extends Base[T, S](_bud) with Event[T, S] {

    override protected[rescala] def reevaluate(ticket: S#Ticket[S]): ReevaluationResult[Pulse[T], S] = {
      val dt = ticket.dynamic()
      val newPulse = Pulse.tryCatch(expr(dt), onEmpty = NoChange)
      ReevaluationResult.Dynamic(newPulse, dt.collectedDependencies)
    }
  }

  /** the basic method to create static events */
  def static[T, S <: Struct](name: String, dependencies: Reactive[S]*)(calculate: StaticTicket[S] => Pulse[T])(implicit ticket: TurnSource[S]): Event[T, S] = ticket { initTurn =>
    val dependencySet: Set[Reactive[S]] = dependencies.toSet
    initTurn.create(dependencySet) {
      new StaticEvent[T, S](initTurn.makeStructState(Pulse.NoChange, initialIncoming = dependencySet, transient = true), calculate, name) with Disconnectable[S]
    }
  }

  /** create dynamic events */
  def dynamic[T, S <: Struct](dependencies: Reactive[S]*)(expr: DynamicTicket[S] => Option[T])(implicit ticket: TurnSource[S]): Event[T, S] = {
    ticket { initialTurn =>
      initialTurn.create(dependencies.toSet, dynamic = true) {
        new DynamicEvent[T, S](initialTurn.makeStructState(Pulse.NoChange, transient = true), expr.andThen(Pulse.fromOption)) with Disconnectable[S]
      }
    }
  }

  def change[A, S <: Struct](signal: Signal[A, S])(implicit ticket: TurnSource[S]): Event[Diff[A], S] = ticket { initTurn =>
    val dependencySet: Set[Reactive[S]] = Set(signal)
    initTurn.create(dependencySet) {
      new ChangeEvent[A, S](initTurn.makeStructState(Pulse.NoChange, initialIncoming = dependencySet, transient = true), signal) with Disconnectable[S]
    }
  }
}
