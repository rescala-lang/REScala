package rescala.propagation

import rescala.graph._
import rescala.reactives.{Event, Signal}

import scala.language.implicitConversions

final class DynamicTicket[S <: Struct](val turn: Turn[S], val ticket: S#Ticket[S]) {
  private[rescala] var collectedDependencies: Set[Reactive[S]] = Set.empty

  def depend[A](reactive: Signal[A, S]): A = {
    collectedDependencies += reactive
    turn.dynamicDependencyInteraction(reactive)
    reactive.pulse(ticket).get
  }

  def depend[A](reactive: Event[A, S]): Option[A] = {
    collectedDependencies += reactive
    turn.dynamicDependencyInteraction(reactive)
    reactive.pulse(ticket).toOption
  }
}

class StaticTicket[S <: Struct](val turn: Turn[S],  val ticket: S#Ticket[S]) {
}
