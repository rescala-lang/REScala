package rescala.propagation

import rescala.graph._
import rescala.reactives.{Event, Signal}

final class ReevaluationTicket[S <: Struct](val turn: Turn[S]) {
  private[rescala] var collectedDependencies: Set[Reactive[S]] = Set.empty

  def depend[A](reactive: Signal[A, S]): A = {
    collectedDependencies += reactive
    turn.dynamicDependencyInteraction(reactive)
    reactive.pulse(turn).get
  }

  def depend[A](reactive: Event[A, S]): Option[A] = {
    collectedDependencies += reactive
    turn.dynamicDependencyInteraction(reactive)
    reactive.pulse(turn).toOption
  }
}
