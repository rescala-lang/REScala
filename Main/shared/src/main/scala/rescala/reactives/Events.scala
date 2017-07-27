package rescala.reactives

import rescala.core.{Base, CreationTicket, Disconnectable, DynamicTicket, Pulse, REName, Reactive, ReevaluationResult, StaticTicket, Struct, Turn, ValuePersistency}
import rescala.core.Pulse.NoChange
import rescala.reactives.Signals.Diff

object Events {

  private abstract class StaticEvent[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: StaticTicket[S] => Pulse[T], name: REName)
    extends Base[T, S](_bud, name) with Event[T, S] {
    override protected[rescala] def reevaluate(turn: Turn[S], before: Pulse[T], indeps: Set[Reactive[S]]): ReevaluationResult[S] =
      ReevaluationResult.Static(turn, this, Pulse.tryCatch(expr(turn.makeStaticReevaluationTicket()), onEmpty = NoChange), indeps)
  }

  private abstract class ChangeEvent[T, S <: Struct](_bud: S#State[Pulse[Diff[T]], S], signal: Signal[T, S], name: REName)
    extends Base[Diff[T], S](_bud, name) with Event[Diff[T], S] {
    override protected[rescala] def reevaluate(turn: Turn[S], before: Pulse[Diff[T]], indeps: Set[Reactive[S]]): ReevaluationResult[S] = {
      val pulse = {
        val st = turn.makeStaticReevaluationTicket()
        val from = st.staticBefore(signal)
        val to = st.staticDepend(signal)
        if (from != Pulse.empty && from != to) Pulse.Value(Diff(from, to))
        else Pulse.NoChange
      }
      ReevaluationResult.Static(turn, this, pulse, indeps)
    }
  }

  private abstract class DynamicEvent[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: DynamicTicket[S] => Pulse[T], name: REName) extends Base[T, S](_bud, name) with Event[T, S] {

    override protected[rescala] def reevaluate(turn: Turn[S], before: Pulse[T], indeps: Set[Reactive[S]]): ReevaluationResult[S] = {
      val dt = turn.makeDynamicReevaluationTicket(indeps)
      val newPulse = Pulse.tryCatch(expr(dt), onEmpty = NoChange)
      ReevaluationResult.Dynamic(turn, this, newPulse, dt.indepsAfter, dt.indepsAdded, dt.indepsRemoved)
    }
  }

  /** the basic method to create static events */
  def static[T, S <: Struct](name: String, dependencies: Reactive[S]*)(calculate: StaticTicket[S] => Pulse[T])(implicit maybe: CreationTicket[S]): Event[T, S] = maybe { initTurn =>
    initTurn.create[Pulse[T], Event[T, S]](dependencies.toSet, ValuePersistency.Event) {
      state => new StaticEvent[T, S](state, calculate, name) with Disconnectable[S]
    }
  }

  /** create dynamic events */
  def dynamic[T, S <: Struct](dependencies: Reactive[S]*)(expr: DynamicTicket[S] => Option[T])(implicit maybe: CreationTicket[S]): Event[T, S] = {
    maybe { initialTurn =>
      initialTurn.create[Pulse[T], Event[T, S]](dependencies.toSet, ValuePersistency.Event) {
        state => new DynamicEvent[T, S](state, expr.andThen(Pulse.fromOption), maybe.rename) with Disconnectable[S]
      }
    }
  }

  def change[A, S <: Struct](signal: Signal[A, S])(implicit maybe: CreationTicket[S]): Event[Diff[A], S] = maybe { initTurn =>
    initTurn.create[Pulse[Diff[A]], Event[Diff[A], S]](Set(signal), ValuePersistency.Event) {
      state => new ChangeEvent[A, S](state, signal, maybe.rename) with Disconnectable[S]
    }
  }
}
