package rescala.graph

import rescala.engine.Turn
import rescala.reactives.{Event, Signal}

/* tickets are created by the REScala schedulers, to restrict operations to the correct scopes */

trait AlwaysTicket[S <: Struct] extends Any {
  def turn: Turn[S]
  private[rescala] def before[A](reactive: Pulsing[A, S]): A = {
    turn.dynamicBefore(reactive)
  }
}

sealed trait OutsidePropagationTicket[S <: Struct] extends Any with AlwaysTicket[S]

sealed trait PropagationAndLaterTicket[S <: Struct] extends Any with AlwaysTicket[S] {
  private[rescala] def after[A](reactive: Pulsing[A, S]): A = {
    turn.dynamicAfter(reactive)
  }
}

final class DynamicTicket[S <: Struct] private[rescala] (val turn: Turn[S]) extends PropagationAndLaterTicket[S] {
  private[rescala] var collectedDependencies: Set[Reactive[S]] = Set.empty
  private[rescala] def dynamicDepend[A](reactive: Pulsing[A, S]): A = {
    collectedDependencies += reactive
    after(reactive)
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
    before(reactive).get
  }
  def now[A](reactive: Event[A, S]): Option[A] = {
    before(reactive).toOption
  }
}

final class WrapUpTicket[S <: Struct] private[rescala] (val turn: Turn[S]) extends AnyVal with PropagationAndLaterTicket[S] with OutsidePropagationTicket[S] {
  def now[A](reactive: Signal[A, S]): A = {
    after(reactive).get
  }
  def now[A](reactive: Event[A, S]): Option[A] = {
    after(reactive).toOption
  }
}
