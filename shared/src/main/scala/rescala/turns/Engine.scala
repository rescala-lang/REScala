package rescala.turns

import rescala.graph.{Reactive, State}
import rescala.macros.SignalMacro

import scala.annotation.implicitNotFound
import scala.language.experimental.macros


@implicitNotFound(msg = "could not find a propagation engine, select one from Engines")
trait Engine[S <: State, +TTurn <: Turn[S]] {

  type Signal[+A] = rescala.Signal[A, S]
  type Event[+A] = rescala.Event[A, S]
  def Evt[A](): rescala.Evt[A, S] = rescala.Evt[A, S]()(this)
  def Var[A](v: A): rescala.Var[A, S] = rescala.Var[A, S](v)(this)

  def Signal[A](expression: A): Signal[A] = macro SignalMacro.SignalMacro[A, S]

  /** used for the creation of state inside reactives */
  private[rescala] def bufferFactory: S

  /** creates runs and commits a new turn */
  def plan[R](initialWrites: Reactive[S]*)(admissionPhase: TTurn => R): R

  /** uses the current turn if any or creates a new turn if none */
  def subplan[T](initialWrites: Reactive[S]*)(admissionPhase: TTurn => T): T
}
