package rescala

import rescala.core.{ReSerializable, Struct}
import rescala.macros.MacroTags.{Dynamic, Static}
import rescala.reactives.Source

import scala.language.{existentials, implicitConversions}

/** Rescala has two main abstractions. [[Event]] and [[Signal]] commonly referred to as reactives.
  * Use [[Var]] to create signal sources and [[Evt]] to create event sources.
  *
  * Events and signals can be created from other reactives by using combinators,
  * signals additionally can be created using [[Signal]] expressions.
  *
  * @groupname reactive Type aliases for reactives
  * @groupprio reactive 50
  * @groupdesc reactive Rescala has multiple schedulers and each scheduler provides reactives with different internal state.
  *           To ensure safety, each reactive is parameterized over the type of internal state, represented by the type
  *           parameter. To make usage more convenient, we provide type aliases which hide these internals.
  * @groupname create Create new reactives
  * @groupprio create 100
  * @groupname update Update multiple reactives
  * @groupprio update 200
  * @groupname internal Advanced functions used when extending REScala
  * @groupprio internal 900
  * @groupdesc internal Methods and type aliases for advanced usages, these are most relevant to abstract
  *           over multiple scheduler implementations.
  **/
abstract class RescalaInterface[S <: Struct] {
  // need the import inside of the trait, otherwise scala complains that it is shadowed by rescala.macros
  import scala.language.experimental.macros

  /** @group internal */
  def explicitEngine: rescala.core.Scheduler[S]
  /** @group internal */
  implicit def implicitEngine: rescala.core.Scheduler[S] = explicitEngine

  /** Signals represent time changing values of type A
    * @group reactive */
  final type Signal[+A] = reactives.Signal[A, S]
  /** Events represent discrete occurrences of values of type A
    * @group reactive */
  final type Event[+A] = reactives.Event[A, S]
  /** @group reactive */
  final type Observe = reactives.Observe[S]
  /** @group reactive */
  final type Var[A] = reactives.Var[A, S]
  /** @group reactive */
  final type Evt[A] = reactives.Evt[A, S]
  /** @group internal */
  final type StaticTicket = rescala.core.StaticTicket[S]
  /** @group internal */
  final type DynamicTicket = rescala.core.DynamicTicket[S]
  /** @group internal */
  final type AdmissionTicket = rescala.core.AdmissionTicket[S]
  /** @group internal */
  final type WrapUpTicket = rescala.core.WrapUpTicket[S]
  /** @group internal */
  final type CreationTicket = rescala.core.CreationTicket[S]
  /** @group internal */
  final type Creation = rescala.core.Initializer[S]
  /** @group internal */
  final type Reactive = rescala.core.Reactive[S]
  /** @group internal */
  final type ReSource = rescala.core.ReSource[S]

  /** @group create */
  final def Evt[A](): Evt[A] = reactives.Evt[A, S]()(explicitEngine)

  //  final def Var[A](v: A): Var[A] = reactives.Var[A, S](v)(Ticket.fromEngineImplicit(this))
  //  final def Var[A](): Var[A] = reactives.Var[A, S]()(Ticket.fromEngineImplicit(this))

  /** @group create */
  object Var {
    def apply[A: ReSerializable](v: A)(implicit ct: CreationTicket): Var[A] = reactives.Var[A, S](v)(implicitly, ct)
    def empty[A: ReSerializable](implicit ct: CreationTicket): Var[A] = reactives.Var.empty[A, S]()(implicitly, ct)
  }

  /** @group internal */
  final def static[T](dependencies: ReSource*)(expr: StaticTicket => T)(implicit ct: CreationTicket): Signal[T] = Signals.static(dependencies: _*)(expr)
  /** @group internal */
  final def dynamic[T](dependencies: ReSource*)(expr: DynamicTicket => T)(implicit ct: CreationTicket): Signal[T] = Signals.dynamic(dependencies: _*)(expr)
  /** @group internal */
  final def dynamicE[T](dependencies: ReSource*)(expr: DynamicTicket => Option[T])(implicit ct: CreationTicket): Event[T] = Events.dynamic(dependencies: _*)(expr)

  /** A signal expression can be used to create signals accessing arbitrary other signals.
    * Use the apply method on a signal to access its value inside of a signal expression.
    * {{{
    * val a: Signal[Int]
    * val b: Signal[Int]
    * val result: Signal[String] = Signal { a().toString + b().toString}
    * }}}
    * @group create
    **/
  object Signal {
    final def apply[A](expression: A)(implicit ticket: CreationTicket): Signal[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[A, S, Static, rescala.reactives.Signals.type]
    final def static[A](expression: A)(implicit ticket: CreationTicket): Signal[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[A, S, Static, rescala.reactives.Signals.type]
    final def dynamic[A](expression: A)(implicit ticket: CreationTicket): Signal[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[A, S, Dynamic, rescala.reactives.Signals.type]
  }
  /** Similar to [[Signal]] expressions, but resulting in an event.
    * Accessed events return options depending on whether they fire or not,
    * and the complete result of the expression is an event as well.
    *
    * @see [[Signal]]
    * @group create */
  object Event {
    final def apply[A](expression: Option[A])(implicit ticket: CreationTicket): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[A, S, Static, rescala.reactives.Events.type]
    final def static[A](expression: Option[A])(implicit ticket: CreationTicket): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[A, S, Static, rescala.reactives.Events.type]
    final def dynamic[A](expression: Option[A])(implicit ticket: CreationTicket): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[A, S, Dynamic, rescala.reactives.Events.type]
  }

  implicit def EventOps[T](e: Event[T]): Events.EOps[T, S] = new Events.EOps[T, S](e)


  /** Contains static methods to create Events
    * @group create */
  val Events: reactives.Events.type = reactives.Events
  /** Contains static methods to create Signals
    * @group create */
  val Signals: reactives.Signals.type = reactives.Signals

  /**
    * Executes a transaction.
    *
    * @param initialWrites  All inputs that might be changed by the transaction
    * @param admissionPhase An admission function that may perform arbitrary [[rescala.reactives.Signal.readValueOnce]] reads
    *                       to [[rescala.reactives.Evt.admit]] / [[rescala.reactives.Var.admit]] arbitrary
    *                       input changes that will be applied as an atomic transaction at the end.
    * @tparam R Result type of the admission function
    * @return Result of the admission function
    * @group update
    */
  def transaction[R](initialWrites: ReSource*)(admissionPhase: AdmissionTicket => R): R = {
    explicitEngine.executeTurn(initialWrites.toSet, admissionPhase)
  }

  /**
    * Executes a transaction with WrapUpPhase.
    *
    * @param initialWrites  All inputs that might be changed by the transaction
    * @param admissionPhase An admission function that may perform arbitrary [[rescala.reactives.Signal.readValueOnce]] reads
    *                       to [[rescala.reactives.Evt.admit]] / [[rescala.reactives.Var.admit]] arbitrary
    *                       input changes that will be applied as an atomic transaction at the end.
    *                       The return value of this phase will be passed to the wrapUpPhase
    * @param wrapUpPhase    A wrap-up function that receives the admissionPhase result and may perform arbitrary
    *                       [[rescala.reactives.Signal.readValueOnce]] reads which are
    *                       executed after the update propagation.
    * @tparam I Intermediate Result type passed from admission to wrapup phase
    * @tparam R Final Result type of the wrapup phase
    * @return Result of the wrapup function
    * @group update
    */
  def transactionWithWrapup[I, R](initialWrites: ReSource*)(admissionPhase: AdmissionTicket => I)(wrapUpPhase: (I, WrapUpTicket) => R): R = {
    var res: Option[R] = None
    transaction(initialWrites: _*)(at => {
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
    * @group update
    */
  def update(changes: (Source[S, A], A) forSome { type A } *): Unit = {
    explicitEngine.executeTurn(changes.foldLeft(Set.empty[ReSource]) { case (accu, (source, _)) =>
      assert(!accu.contains(source), s"must not admit multiple values for the same source ($source was assigned multiple times)")
      accu + source
    }, { t =>
      for(change <- changes) admit(t, change)
    })
  }
  private def admit[A](t: AdmissionTicket, change: (Source[S, A], A)) = change._1.admit(change._2)(t)
}
