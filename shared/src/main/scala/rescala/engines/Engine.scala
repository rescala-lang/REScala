package rescala.engines

import rescala.graph.Spores
import rescala.macros.ReactiveMacros
import rescala.propagation.Turn
import rescala.reactives.{Events, Signals}
import rescala.{reactives, propagation}

import scala.annotation.implicitNotFound
import scala.language.experimental.macros


@implicitNotFound(msg = "could not find a propagation engine, select one from Engines")
trait Engine[S <: Spores, +TTurn <: Turn[S]] {

  implicit def _engine: this.type = this

  final type Signal[+A] = reactives.Signal[A, S]
  final type Event[+A] = reactives.Event[A, S]
  final type Var[A] = reactives.Var[A, S]
  final type Evt[A] = reactives.Evt[A, S]
  final type Spores = S
  final type Turn = propagation.Turn[S]
  final type Ticket = rescala.engines.Ticket[S]
  final type Reactive = rescala.graph.Reactive[S]
  final def Evt[A](): Evt[A] = reactives.Evt[A, S]()(this)
  final def Var[A](v: A): Var[A] = reactives.Var[A, S](v)(this)
  final def static[T](dependencies: Reactive*)(expr: Turn => T)(implicit ticket: Ticket): Signal[T] = Signals.static(dependencies: _*)(expr)
  final def dynamic[T](dependencies: Reactive*)(expr: Turn => T)(implicit ticket: Ticket): Signal[T] = Signals.dynamic(dependencies: _*)(expr)
  final def dynamicE[T](dependencies: Reactive*)(expr: Turn => Option[T])(implicit ticket: Ticket): Event[T] = Events.dynamic(dependencies: _*)(expr)

  final def Signal[A](expression: A): Signal[A] = macro ReactiveMacros.SignalMacro[A, S]
  final def Event[A](expression: Option[A]): Event[A] = macro ReactiveMacros.EventMacro[A, S]


  /** used for the creation of state inside reactives */
  private[rescala] def bufferFactory: S

  /** creates runs and commits a new turn */
  def plan[R](initialWrites: Reactive*)(admissionPhase: TTurn => R): R

  /** uses the current turn if any or creates a new turn if none */
  def subplan[T](initialWrites: Reactive*)(admissionPhase: TTurn => T): T
}
