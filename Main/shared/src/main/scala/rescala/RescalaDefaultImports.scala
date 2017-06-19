package rescala

import rescala.graph.{ReSerializable, Struct}
import rescala.macros.ReactiveMacros
import rescala.reactives.Source

import scala.language.existentials


abstract class RescalaDefaultImports[S <: Struct] {
  // need the import inside of the trait, otherwise scala complains that it is shadowed by rescala.macros
  import scala.language.experimental.macros

  def explicitEngine: rescala.engine.Engine[S]
  implicit def implicitEngine: rescala.engine.Engine[S] = explicitEngine

  final type Observe = reactives.Observe[S]
  final type Signal[+A] = reactives.Signal[A, S]
  final type Event[+A] = reactives.Event[A, S]
  final type Var[A] = reactives.Var[A, S]
  final type Evt[A] = reactives.Evt[A, S]
  final type Turn = rescala.engine.Turn[S]
  final type StaticTicket = rescala.graph.StaticTicket[S]
  final type DynamicTicket = rescala.graph.DynamicTicket[S]
  final type AdmissionTicket = rescala.graph.AdmissionTicket[S]
  final type WrapUpTicket = rescala.graph.WrapUpTicket[S]
  final type OutsidePropagationTicket = rescala.graph.OutsidePropagationTicket[S]
  final type PropagationAndLaterTicket = rescala.graph.PropagationAndLaterTicket[S]
  final type AlwaysTicket = rescala.graph.AlwaysTicket[S]
  final type TurnSource = rescala.graph.CreationTicket[S]
  final type TicketOrEngine = rescala.graph.CreationTicket[S]
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


  implicit def everythingIsSerializable[A]: ReSerializable[A] = null



  final protected[rescala] def noWrapUp[R](intermediate: R, turn: WrapUpTicket): R = intermediate
  /**
    * Executes a transaction.
    *
    * @param initialWrites All inputs that might be changed by the transaction
    * @param admissionPhase An admission function that may perform arbitrary [[rescala.reactives.Signal.now]] reads
    *                       to [[rescala.reactives.Evt.admit]] / [[rescala.reactives.Var.admit]] arbitrary
    *                       input changes that will be applied as an atomic transaction at the end.
    * @tparam R Result type of the admission function
    * @return Result of the admission function
    */
  def transaction[R](initialWrites: Reactive*)(admissionPhase: AdmissionTicket => R): R = {
    explicitEngine.executeTurn(initialWrites, admissionPhase, noWrapUp[R])
  }

  /**
    * Executes a transaction with WrapUpPhase.
    *
    * @param initialWrites All inputs that might be changed by the transaction
    * @param admissionPhase An admission function that may perform arbitrary [[rescala.reactives.Signal.now]] reads
    *                       to [[rescala.reactives.Evt.admit]] / [[rescala.reactives.Var.admit]] arbitrary
    *                       input changes that will be applied as an atomic transaction at the end.
    *                       The return value of this phase will be passed to the wrapUpPhase
    * @param wrapUpPhase A wrap-up function that receives the admissionPhase result and may perform arbitrary
    *                    [[rescala.reactives.Signal.before]] and [[rescala.reactives.Signal.now]] reads which are
    *                    executed after the update propagation.
    * @tparam I Intermediate Result type passed from admission to wrapup phase
    * @tparam R Final Result type of the wrapup phase
    * @return Result of the wrapup function
    */
  def transactionWithWrapup[I, R](initialWrites: Reactive*)(admissionPhase: AdmissionTicket => I)(wrapUpPhase: (I, WrapUpTicket) => R): R = {
    explicitEngine.executeTurn(initialWrites, admissionPhase, wrapUpPhase)
  }

  /**
    * Atomically changes multiple inputs in a single [[transaction]]
    *
    * @param changes the changes to perform, i.e., (i1 -> v1, i2 -> v2, ...)
    * @return Result of the admission function
    */
  def update(changes: (Source[A, S], A) forSome { type A } *): Unit = {
    explicitEngine.executeTurn(changes.map(_._1), { t =>
      def apply[A](change: (Source[A, S], A)) = change._1.admit(change._2)(t)
      for(change <- changes) apply(change)
    }, noWrapUp[Unit])
  }
}
