package rescala.propagation

import rescala.graph._

class ReevaluationTicket[S <: Struct](val turn: Turn[S], val issuer: Reactive[S]) {
  private[rescala] var collectedDependencies: Set[Reactive[S]] = Set.empty

  def depend[A](reactive: Stateful[A, S]): A = {
    collectedDependencies += reactive
    reactive.depend(turn, issuer)
  }

  def depend[A](reactive: PulseOption[A, S]): Option[A] = {
    collectedDependencies += reactive
    reactive.depend(turn, issuer)
  }
}
