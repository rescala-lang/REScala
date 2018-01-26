package rescala.reactives

import rescala.core.Pulse.{Exceptional, NoChange, Value}
import rescala.core.Result.NoValue
import rescala.core._
import rescala.reactives.Signals.Diff

/** Functions to construct events, you probably want to use the operators on [[Event]] for a nicer API. */
object Events {

  /** the basic method to create static events */
  def staticNamed[T, S <: Struct](name: String, dependencies: ReSource[S]*)(calculate: StaticTicket[S] => Pulse[T])(implicit ticket: CreationTicket[S]): Event[T, S] = ticket { initTurn =>
    initTurn.create[Pulse[T], StaticEvent[T, S]](dependencies.toSet, ValuePersistency.Event) {
      state => new StaticEvent[T, S](state, calculate, name) with DisconnectableImpl[S]
    }
  }

  /** Creates static events */
  def static[T, S <: Struct](dependencies: ReSource[S]*)(calculate: StaticTicket[S] => Option[T])(implicit ticket: CreationTicket[S]): Event[T, S] =
    staticNamed(ticket.rename.name, dependencies: _*)(st => Pulse.fromOption(calculate(st)))

  /** Creates dynamic events */
  def dynamic[T, S <: Struct](dependencies: ReSource[S]*)(expr: ReevTicket[S] => Option[T])(implicit ticket: CreationTicket[S]): Event[T, S] = {
    ticket { initialTurn =>
      initialTurn.create[Pulse[T], DynamicEvent[T, S]](dependencies.toSet, ValuePersistency.DynamicEvent) {
        state => new DynamicEvent[T, S](state, expr.andThen(Pulse.fromOption), ticket.rename) with DisconnectableImpl[S]
      }
    }
  }

  /** Creates change events */
  def change[A, S <: Struct](signal: Signal[A, S])(implicit ticket: CreationTicket[S]): Event[Diff[A], S] = ticket { initTurn =>
    initTurn.create[Pulse[Diff[A]], ChangeEvent[A, S]](Set(signal), ValuePersistency.ChangeEvent) {
      state => new ChangeEvent[A, S](state, signal, ticket.rename) with DisconnectableImpl[S]
    }
  }
}

private abstract class StaticEvent[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: StaticTicket[S] => Pulse[T], name: REName)
  extends Base[Pulse[T], S](_bud, name) with Event[T, S] {
  override protected[rescala] def reevaluate(dt: ReevTicket[S], before: Pulse[T]): Result[Value, S] =
    Result.fromPulse(Pulse.tryCatch(expr(dt), onEmpty = NoChange))
}


private abstract class ChangeEvent[T, S <: Struct](_bud: S#State[Pulse[Diff[T]], S], signal: Signal[T, S], name: REName)
  extends Base[Pulse[Diff[T]], S](_bud, name) with Event[Diff[T], S] {
  override protected[rescala] def reevaluate(st: ReevTicket[S], before: Pulse[Diff[T]]): Result[Value, S] = {
    val to: Pulse[T] = st.readStatic(signal)
    if (to == Pulse.empty) return NoValue(propagate = false)
    before match {
      case Value(u) =>
        val from = u.to
        if (from == to) NoValue(propagate = false)
        else Result.fromPulse(Pulse.Value(Diff(from, to)))
      case NoChange =>
        val res = Diff(Pulse.empty, st.readStatic(signal))
        Result.Static(Pulse.Value(res),propagate = false)
      case x@Exceptional(_) =>
        Result.fromPulse(x) //should not happen, change does not acutally access other pulses
    }
  }
}

private abstract class DynamicEvent[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: ReevTicket[S] => Pulse[T], name: REName) extends Base[Pulse[T], S](_bud, name) with Event[T, S] {

  override protected[rescala] def reevaluate(dt: ReevTicket[S], before: Pulse[T]): Result[Value, S] = {
    dt.trackDependencies()
    Result.dynamicFromPulse(Pulse.tryCatch(expr(dt), onEmpty = NoChange))
  }
}
