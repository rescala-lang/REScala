package rescala.graph

import rescala.engine.Turn
import rescala.reactives.{Event, Signal}

/* tickets are created by the REScala schedulers, to restrict operations to the correct scopes */

// thoughts regarding now, before and after:
// before: can be used at any time during turns. Thus its parameter should accept implicit turns only.
//         However, we want to be able to invoke .before inside, e.g., the closure of a .fold, where the turn
//         is not present in the scope. Thus before also accepts engines, but dynamically checks, that this engine
//         has a current turn set. This could probably be ensured statically by making sure that every reactive
//         definition site somehow provides the reevaluating ticket as an implicit in the scope, but I'm not sure
//         if this is possible without significant syntactical inconvenience.
//  after: falls under the same considerations as before, with the added exception that it should only accept
//         turns that completed their admission phase and started their propagation phase. This is currently not
//         checked at all, but could also be ensured statically the same as above.
//    now: can be used inside turns only during admission and wrapup, or outside of turns at all times. Since during
//         admission and wrapup, a corresponding OutsidePropagationTicket is in scope, it accepts these tickets as
//         high priority implicit parameters. In case such a ticket is not available, it accepts engines, and then
//         dynamically checks that the engine does NOT have a current turn (in the current thread context). I think
//         this cannot be ensured statically, as users can always hide implicitly available current turns.

trait AlwaysTicket[S <: Struct] extends Any {
  def turn: Turn[S]
}

sealed trait OutsidePropagationTicket[S <: Struct] extends Any with AlwaysTicket[S]

sealed trait PropagationAndLaterTicket[S <: Struct] extends Any with AlwaysTicket[S]

final class DynamicTicket[S <: Struct] private[rescala] (val turn: Turn[S]) extends PropagationAndLaterTicket[S] {
  private[rescala] var collectedDependencies: Set[Reactive[S]] = Set.empty
  private[rescala] def dynamicDepend[A](reactive: Pulsing[A, S]): A = {
    collectedDependencies += reactive
    turn.dynamicAfter(reactive)
  }

  def before[A](reactive: Signal[A, S]): A = {
    turn.dynamicBefore(reactive).get
  }

  def depend[A](reactive: Signal[A, S]): A = {
    dynamicDepend(reactive).get
  }

  def depend[A](reactive: Event[A, S]): Option[A] = {
    dynamicDepend(reactive).toOption
  }

}

final class StaticTicket[S <: Struct] private[rescala] (val turn: Turn[S]) extends AnyVal with PropagationAndLaterTicket[S] {
  private[rescala] def staticBefore[A](reactive: Pulsing[A, S]): A = {
    turn.staticBefore(reactive)
  }
  private[rescala] def staticDepend[A](reactive: Pulsing[A, S]): A = {
    turn.staticAfter(reactive)
  }
}

final class AdmissionTicket[S <: Struct] private[rescala] (val turn: Turn[S]) extends AnyVal with OutsidePropagationTicket[S] {
  def now[A](reactive: Signal[A, S]): A = {
    turn.dynamicBefore(reactive).get
  }
  def now[A](reactive: Event[A, S]): Option[A] = {
    turn.dynamicBefore(reactive).toOption
  }
}

final class WrapUpTicket[S <: Struct] private[rescala] (val turn: Turn[S]) extends AnyVal with PropagationAndLaterTicket[S] with OutsidePropagationTicket[S] {
  def now[A](reactive: Signal[A, S]): A = {
    turn.dynamicAfter(reactive).get
  }
  def now[A](reactive: Event[A, S]): Option[A] = {
    turn.dynamicAfter(reactive).toOption
  }
}
