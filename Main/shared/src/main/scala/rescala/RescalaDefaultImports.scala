package rescala

import rescala.graph.Struct
import rescala.macros.ReactiveMacros


trait RescalaDefaultImports[S <: Struct] {
  // need the import inside of the trait, otherwise scala complains that it is shadowed by rescala.macros
  import scala.language.experimental.macros

  implicit def Engine: rescala.engine.Engine[S, Turn]

  final type Observe = reactives.Observe[S]
  final type Signal[+A] = reactives.Signal[A, S]
  final type Event[+A] = reactives.Event[A, S]
  final type Var[A] = reactives.Var[A, S]
  final type Evt[A] = reactives.Evt[A, S]
  final type Turn = propagation.Turn[S]
  final type Ticket = rescala.engine.TurnSource[S]
  final type Reactive = rescala.graph.Reactive[S]
  final def Evt[A](): Evt[A] = reactives.Evt[A, S]()

  //  final def Var[A](v: A): Var[A] = reactives.Var[A, S](v)(Ticket.fromEngineImplicit(this))
  //  final def Var[A](): Var[A] = reactives.Var[A, S]()(Ticket.fromEngineImplicit(this))

  object Var {
    def apply[A](v: A): Var[A] = reactives.Var[A, S](v)
    def empty[A]: Var[A] = reactives.Var.empty[A, S]
  }



  final def static[T](dependencies: Reactive*)(expr: Turn => T)(implicit ticket: Ticket): Signal[T] = Signals.static(dependencies: _*)(expr)
  final def dynamic[T](dependencies: Reactive*)(expr: Turn => T)(implicit ticket: Ticket): Signal[T] = Signals.dynamic(dependencies: _*)(expr)
  final def dynamicE[T](dependencies: Reactive*)(expr: Turn => Option[T])(implicit ticket: Ticket): Event[T] = Events.dynamic(dependencies: _*)(expr)

  final def Signal[A](expression: A): Signal[A] = macro ReactiveMacros.SignalMacro[A, S]
  final def Event[A](expression: Option[A]): Event[A] = macro ReactiveMacros.EventMacro[A, S]

  val Events = reactives.Events
  val Signals = reactives.Signals
}
