package rescala.graph

import rescala.engine.Turn
import rescala.reactives.{Event, Signal}

trait AlwaysTicket[S <: Struct] extends Any {
  def turn: Turn[S]
  private[rescala] def before[A](reactive: Pulsing[A, S]): A = {
    turn.dynamicBefore(reactive)
  }
}

trait OutsidePropagationTicket[S <: Struct] extends Any with AlwaysTicket[S] {
  private[rescala] def now[A](reactive: Pulsing[A, S]): A
}

trait PropagationAndLaterTicket[S <: Struct] extends Any with AlwaysTicket[S] {
  private[rescala] def after[A](reactive: Pulsing[A, S]): A = {
    turn.dynamicAfter(reactive)
  }
}

final class DynamicTicket[S <: Struct](val turn: Turn[S]) extends PropagationAndLaterTicket[S] {
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

class StaticTicket[S <: Struct](val turn: Turn[S]) extends AnyVal with PropagationAndLaterTicket[S] {
  private[rescala] def staticBefore[A](reactive: Pulsing[A, S]): A = {
    turn.staticBefore(reactive)
  }
  private[rescala] def staticDepend[A](reactive: Pulsing[A, S]): A = {
    turn.staticAfter(reactive)
  }
}

class AdmissionTicket[S <: Struct](val turn: Turn[S]) extends AnyVal with OutsidePropagationTicket[S] {
  override private[rescala] def now[A](reactive: Pulsing[A, S]): A = {
    before(reactive)
  }
}

class WrapUpTicket[S <: Struct](val turn: Turn[S]) extends AnyVal with PropagationAndLaterTicket[S] with OutsidePropagationTicket[S] {
  override private[rescala] def now[A](reactive: Pulsing[A, S]): A = {
    after(reactive)
  }
}
