package rescala.engines

import rescala.graph.Struct
import rescala.macros.ReactiveMacros
import rescala.propagation.Turn
import rescala.reactives.{Events, Signals}
import rescala.{propagation, reactives}

import scala.annotation.implicitNotFound
import scala.language.experimental.macros

/**
  * Propagation engine that defines the basic data-types available to the user and creates turns for propagation handling
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  * @tparam TTurn Turn type used by the engine
  */
@implicitNotFound(msg = "could not find a propagation engine, select one from Engines")
trait Engine[S <: Struct, +TTurn <: Turn[S]] {

  implicit def Engine: this.type = this

  final type Signal[+A] = reactives.Signal[A, S]
  final type Event[+A] = reactives.Event[A, S]
  final type Var[A] = reactives.Var[A, S]
  final type Evt[A] = reactives.Evt[A, S]
  final type Turn = propagation.Turn[S]
  final type Ticket = rescala.engines.Ticket[S]
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

  /**
    * Creates and executes a full turn by running through all of its phases.
    *
    * @param initialWrites Initially modified reactive values that are the source of the turn's propagation
    * @param admissionPhase Function executed between the preparation and the propagation phase
    * @tparam R Result type of the admission function
    * @return Result of the admission function
    */
  def plan[R](initialWrites: Reactive*)(admissionPhase: TTurn => R): R

  /**
    * If there is already a current turn running, the given function is applied onto it. Otherwise, a new full turn
    * is executed with all of its phases.
    *
    * @param initialWrites Initially modified reactive values that are the source of a new turn's propagation
    * @param admissionPhase Function executed on the existing turn or between the preparation and the propagation phase of a new turn.
    * @tparam T Result type of the admission function
    * @return Result of the admission function
    */
  def subplan[T](initialWrites: Reactive*)(admissionPhase: TTurn => T): T
}
