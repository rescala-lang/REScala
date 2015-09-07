package rescala.turns

import rescala.{Events, Signals, Signal}
import rescala.graph.{Reactive, Spores}
import rescala.macros.ReactiveMacros

import scala.annotation.implicitNotFound
import scala.language.experimental.macros


@implicitNotFound(msg = "could not find a propagation engine, select one from Engines")
trait Engine[S <: Spores, +TTurn <: Turn[S]] {

  type Signal[+A] = rescala.Signal[A, S]
  type Event[+A] = rescala.Event[A, S]
  type Var[A] = rescala.Var[A, S]
  type Evt[A] = rescala.Evt[A, S]
  type Spore = S
  type Turn = rescala.turns.Turn[S]
  def Evt[A](): Evt[A] = rescala.Evt[A, S]()(this)
  def Var[A](v: A): Var[A] = rescala.Var[A, S](v)(this)
  def dynamic[T](dependencies: Reactive[S]*)(expr: Turn => T)(implicit ticket: Ticket[S]): Signal[T] = Signals.dynamic(dependencies: _*)(expr)
  def dynamicE[T](dependencies: Reactive[S]*)(expr: Turn => Option[T])(implicit ticket: Ticket[S]): Event[T] = Events.dynamic(dependencies: _*)(expr)

  def Signal[A](expression: A): Signal[A] = macro ReactiveMacros.SignalMacro[A, S]
  def Event[A](expression: Option[A]): Event[A] = macro ReactiveMacros.EventMacro[A, S]


  /** used for the creation of state inside reactives */
  private[rescala] def bufferFactory: S

  /** creates runs and commits a new turn */
  def plan[R](initialWrites: Reactive[S]*)(admissionPhase: TTurn => R): R

  /** uses the current turn if any or creates a new turn if none */
  def subplan[T](initialWrites: Reactive[S]*)(admissionPhase: TTurn => T): T
}
