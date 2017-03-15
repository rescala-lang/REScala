package rescala

import rescala.graph.Struct
import rescala.macros.ReactiveMacros


abstract class RescalaDefaultImports[S <: Struct] {
  // need the import inside of the trait, otherwise scala complains that it is shadowed by rescala.macros
  import scala.language.experimental.macros

  def explicitEngine: rescala.engine.Engine[S, Turn]
  implicit def implicitEngine: rescala.engine.Engine[S, Turn] = explicitEngine

  final type Observe = reactives.Observe[S]
  final type Signal[+A] = reactives.Signal[A, S]
  final type Event[+A] = reactives.Event[A, S]
  final type Var[A] = reactives.Var[A, S]
  final type Evt[A] = reactives.Evt[A, S]
  final type Turn = propagation.Turn[S]
  final type StaticTicket = propagation.StaticTicket[S]
  final type DynamicTicket = propagation.DynamicTicket[S]
  final type TurnSource = rescala.engine.TurnSource[S]
  final type Reactive = rescala.graph.Reactive[S]
  final def Evt[A](): Evt[A] = reactives.Evt[A, S]()(explicitEngine)

  //  final def Var[A](v: A): Var[A] = reactives.Var[A, S](v)(Ticket.fromEngineImplicit(this))
  //  final def Var[A](): Var[A] = reactives.Var[A, S]()(Ticket.fromEngineImplicit(this))

  object Var {
    def apply[A](v: A): Var[A] = reactives.Var[A, S](v)(explicitEngine)
    def empty[A]: Var[A] = reactives.Var.empty[A, S]()(explicitEngine)
  }



  final def static[T](dependencies: Reactive*)(expr: StaticTicket => T)(implicit turnSource: TurnSource): Signal[T] = Signals.static(dependencies: _*)(expr)
  final def dynamic[T](dependencies: Reactive*)(expr: DynamicTicket => T)(implicit turnSource: TurnSource): Signal[T] = Signals.dynamic(dependencies: _*)(expr)
  final def dynamicE[T](dependencies: Reactive*)(expr: DynamicTicket => Option[T])(implicit turnSource: TurnSource): Event[T] = Events.dynamic(dependencies: _*)(expr)

  final def Signal[A](expression: A): Signal[A] = macro ReactiveMacros.SignalMacro[A, S]
  final def Event[A](expression: Option[A]): Event[A] = macro ReactiveMacros.EventMacro[A, S]

  val Events = reactives.Events
  val Signals = reactives.Signals
}
