package rescala.graph

import rescala.engine.Turn
import rescala.reactives.{Event, Signal}

import scala.language.implicitConversions

final class DynamicTicket[S <: Struct](val turn: Turn[S]) {
  private[rescala] var collectedDependencies: Set[Reactive[S]] = Set.empty

  def depend[A](reactive: Signal[A, S]): A = {
    access(reactive).get
  }

  def depend[A](reactive: Event[A, S]): Option[A] = {
    access(reactive).toOption
  }

  private def access[A](reactive: Pulsing[A, S]): A = {
    collectedDependencies += reactive
    turn.dynamicDependencyInteraction(reactive)
    turn.after(reactive)
  }
}

class StaticTicket[S <: Struct](val turn: Turn[S])
