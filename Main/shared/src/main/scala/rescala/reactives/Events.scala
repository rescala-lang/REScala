package rescala.reactives

import rescala.core.Pulse.NoChange
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
  def dynamic[T, S <: Struct](dependencies: ReSource[S]*)(expr: DynamicTicket[S] => Option[T])(implicit ticket: CreationTicket[S]): Event[T, S] = {
    ticket { initialTurn =>
      initialTurn.create[Pulse[T], DynamicEvent[T, S]](dependencies.toSet, ValuePersistency.DynamicEvent) {
        state => new DynamicEvent[T, S](state, expr.andThen(Pulse.fromOption), ticket.rename) with DisconnectableImpl[S]
      }
    }
  }

  /** Creates change events */
  def change[A, S <: Struct](signal: Signal[A, S])(implicit ticket: CreationTicket[S]): Event[Diff[A], S] = ticket { initTurn =>
    initTurn.create[Pulse[Diff[A]], ChangeEvent[A, S]](Set(signal), ValuePersistency.Event) {
      state => new ChangeEvent[A, S](state, signal, ticket.rename) with DisconnectableImpl[S]
    }
  }
}

private abstract class StaticEvent[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: StaticTicket[S] => Pulse[T], name: REName)
  extends Base[Pulse[T], S](_bud, name) with Event[T, S] {
  override protected[rescala] def reevaluate(turn: Turn[S], before: Pulse[T], indeps: Set[ReSource[S]]): ReevaluationResult[Value, S] =
    ReevaluationResult.StaticPulse(Pulse.tryCatch(expr(turn.makeStaticReevaluationTicket()), onEmpty = NoChange), indeps)
}

private abstract class ChangeEvent[T, S <: Struct](_bud: S#State[Pulse[Diff[T]], S], signal: Signal[T, S], name: REName)
  extends Base[Pulse[Diff[T]], S](_bud, name) with Event[Diff[T], S] {
  override protected[rescala] def reevaluate(turn: Turn[S], before: Pulse[Diff[T]], indeps: Set[ReSource[S]]): ReevaluationResult[Value, S] = {
    val pulse = {
      val st = turn.makeStaticReevaluationTicket()
      val from = st.staticBefore(signal)
      val to = st.staticDependPulse(signal)
      if (from != Pulse.empty && to != Pulse.empty && from != to) Pulse.Value(Diff(from, to))
      else Pulse.NoChange
    }
    ReevaluationResult.StaticPulse(pulse, indeps)
  }
}

private abstract class DynamicEvent[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: DynamicTicket[S] => Pulse[T], name: REName) extends Base[Pulse[T], S](_bud, name) with Event[T, S] {

  override protected[rescala] def reevaluate(turn: Turn[S], before: Pulse[T], indeps: Set[ReSource[S]]): ReevaluationResult[Value, S] = {
    val dt = turn.makeDynamicReevaluationTicket(indeps)
    val newPulse = Pulse.tryCatch(expr(dt), onEmpty = NoChange)
    ReevaluationResult.DynamicPulse(newPulse, dt.indepsAfter, dt.indepsAdded, dt.indepsRemoved)
  }
}
