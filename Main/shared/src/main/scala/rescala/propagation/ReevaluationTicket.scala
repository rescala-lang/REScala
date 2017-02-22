package rescala.propagation

import rescala.graph._

final class ReevaluationTicket[S <: Struct](val turn: Turn[S]) {
  private[rescala] var collectedDependencies: Set[Reactive[S]] = Set.empty

  def depend[A](reactive: Stateful[A, S]): A = {
    collectedDependencies += reactive
    turn.dynamicDependencyInteraction(reactive)
    reactive.regRead(turn)
  }

  def depend[A](reactive: PulseOption[A, S]): Option[A] = {
    collectedDependencies += reactive
    turn.dynamicDependencyInteraction(reactive)
    reactive.regRead(turn)
  }
}
