package rescala

import rescala.core.{ReSerializable, Struct}
import rescala.macros.ReactiveMacros
import rescala.reactives.Source

import scala.language.existentials

abstract class RescalaInterface[S <: Struct] {
  // need the import inside of the trait, otherwise scala complains that it is shadowed by rescala.macros
  import scala.language.experimental.macros

  def explicitEngine: rescala.core.Engine[S]
  implicit def implicitEngine: rescala.core.Engine[S] = explicitEngine

  /** Signals represent time changing values of type A */
  final type Signal[+A] = reactives.Signal[A, S]
  /** Events represent discrete occurences of values of type A */
  final type Event[+A] = reactives.Event[A, S]
  final type Observe = reactives.Observe[S]
  final type Var[A] = reactives.Var[A, S]
  final type Evt[A] = reactives.Evt[A, S]
  final type StaticTicket = rescala.core.StaticTicket[S]
  final type DynamicTicket = rescala.core.DynamicTicket[S]
  final type AdmissionTicket = rescala.core.AdmissionTicket[S]
  final type WrapUpTicket = rescala.core.WrapUpTicket[S]
  final type CreationTicket = rescala.core.CreationTicket[S]
  final type Creation = rescala.core.Creation[S]
  final type Reactive = rescala.core.Reactive[S]
  final type ReSource = rescala.core.ReSource[S]

  final def Evt[A](): Evt[A] = reactives.Evt[A, S]()(explicitEngine)

  //  final def Var[A](v: A): Var[A] = reactives.Var[A, S](v)(Ticket.fromEngineImplicit(this))
  //  final def Var[A](): Var[A] = reactives.Var[A, S]()(Ticket.fromEngineImplicit(this))

  object Var {
    def apply[A: ReSerializable](v: A)(implicit ct: CreationTicket): Var[A] = reactives.Var[A, S](v)(implicitly, ct)
    def empty[A: ReSerializable](implicit ct: CreationTicket): Var[A] = reactives.Var.empty[A, S]()(implicitly, ct)
  }

  final def static[T](dependencies: ReSource*)(expr: StaticTicket => T)(implicit ct: CreationTicket): Signal[T] = Signals.static(dependencies: _*)(expr)
  final def dynamic[T](dependencies: ReSource*)(expr: DynamicTicket => T)(implicit ct: CreationTicket): Signal[T] = Signals.dynamic(dependencies: _*)(expr)
  final def dynamicE[T](dependencies: ReSource*)(expr: DynamicTicket => Option[T])(implicit ct: CreationTicket): Event[T] = Events.dynamic(dependencies: _*)(expr)

  /** A signal expression can be used to create signals accessing arbitrary other signals.
    * Use the apply method on a signal to access its value inside of a signal expression.
    * {{{
    * val a: Signal[Int]
    * val b: Signal[Int]
    * val result: Signal[String] = Signal { a().toString + b().toString}
    * }}}
    */
  final def Signal[A](expression: A)(implicit ticket: CreationTicket): Signal[A] = macro ReactiveMacros.SignalMacro[A, S]
  final def Event[A](expression: Option[A])(implicit ticket: CreationTicket): Event[A] = macro ReactiveMacros.EventMacro[A, S]

  val Events = reactives.Events
  val Signals = reactives.Signals

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
  def transaction[R](initialWrites: ReSource*)(admissionPhase: AdmissionTicket => R): R = {
    explicitEngine.executeTurn(initialWrites, admissionPhase)
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
    *                    [[rescala.reactives.Signal.now]] reads which are
    *                    executed after the update propagation.
    * @tparam I Intermediate Result type passed from admission to wrapup phase
    * @tparam R Final Result type of the wrapup phase
    * @return Result of the wrapup function
    */
  def transactionWithWrapup[I, R](initialWrites: ReSource*)(admissionPhase: AdmissionTicket => I)(wrapUpPhase: (I, WrapUpTicket) => R): R = {
    var res: Option[R] = None
    explicitEngine.executeTurn(initialWrites, at => {
      val apr: I = admissionPhase(at)
      at.wrapUp = wut => { res = Some(wrapUpPhase(apr, wut))}
    })
    res.get
  }

  /**
    * Atomically changes multiple inputs in a single [[transaction]]
    *
    * @param changes the changes to perform, i.e., (i1 -> v1, i2 -> v2, ...)
    * @return Result of the admission function
    */
  def update(changes: (Source[A, S], A) forSome { type A } *): Unit = {
    explicitEngine.executeTurn(changes.map(_._1), { t =>
      def admit[A](change: (Source[A, S], A)) = change._1.admit(change._2)(t)
      for(change <- changes) admit(change)
    })
  }
}
