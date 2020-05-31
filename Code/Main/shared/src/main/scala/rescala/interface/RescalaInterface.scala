package rescala.interface

import rescala.core.{Base, Initializer, Interp, Pulse, REName, Scheduler, Struct}
import rescala.macros.MacroTags.{Dynamic, Static}
import rescala.reactives
import rescala.reactives.{DefaultImplementations, Signals, Source}


object RescalaInterface {
  def interfaceFor[S <: Struct](someScheduler: Scheduler[S]): RescalaInterface[S] = new RescalaInterface[S] {
    override def scheduler: Scheduler[S] = someScheduler
    override def toString: String = s"Interface($scheduler)"
  }
}

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
trait RescalaInterface[S <: Struct] extends Aliases[S] {
  /** @group internal */
  def scheduler: rescala.core.Scheduler[S]
  /** @group internal */
  implicit def implicitScheduler: rescala.core.Scheduler[S] = scheduler

  /** @group internal */
  implicit def rescalaAPI: RescalaInterface[S] = this

  /** @group create */
  final def Evt[A]()(implicit ticket: CreationTicket): Evt[A] = {
    ticket.createSource[Pulse[A], Evt[A]](Initializer.Event)(init => {
      new reactives.Evt[A, S](init, ticket.rename) {
        override val rescalaAPI: RescalaInterface[S] = RescalaInterface.this
      }
    }: Evt[A])
  }


  /** Creates new [[Var]]s
    * @group create */
  object Var {
    abstract class VarImpl[A] private[rescala](initialState: rescala.reactives.Signals.Sstate[A, S], name: REName)
      extends Base[Pulse[A], S](initialState, name) with rescala.reactives.Var[A, S] with Signal[A]
    with rescala.reactives.Signals.SignalResource[A, S] with Interp[A, S]

      def apply[T](initval: T)(implicit ticket: CreationTicket): VarImpl[T] = fromChange(Pulse.Value(initval))
      def empty[T](implicit ticket: CreationTicket): VarImpl[T] = fromChange(Pulse.empty)
      private[this] def fromChange[T](change: Pulse[T])(implicit ticket: CreationTicket): VarImpl[T] = {
        ticket.createSource[Pulse[T], VarImpl[T]](Initializer.InitializedSignal(change))(new VarImpl[T](_, ticket.rename){
          override val rescalaAPI: RescalaInterface[S] = RescalaInterface.this
          override def interpret(v: Pulse[T]): T = v.get
          override val innerDerived = this
        })
      }
  }

  /** @group internal */
  object NoMacro {
    /** @group internal */
    final def static[T](dependencies: ReSource*)(expr: StaticTicket => T)(implicit ct: CreationTicket): Signal[T] = Signals.static(dependencies: _*)(expr)
    /** @group internal */
    final def dynamic[T](dependencies: ReSource*)(expr: DynamicTicket => T)(implicit ct: CreationTicket): Signal[T] = Signals.dynamic(dependencies: _*)(expr)
    /** @group internal */
    final def dynamicE[T](dependencies: ReSource*)(expr: DynamicTicket => Option[T])(implicit ct: CreationTicket): Event[T] = Events.dynamic(dependencies: _*)(expr)
  }

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
    def rescalaAPI: RescalaInterface.this.type = RescalaInterface.this
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
    def rescalaAPI: RescalaInterface.this.type = RescalaInterface.this
    final def apply[A](expression: Option[A])(implicit ticket: CreationTicket): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[A, S, Static, rescala.reactives.Events.type]
    final def static[A](expression: Option[A])(implicit ticket: CreationTicket): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[A, S, Static, rescala.reactives.Events.type]
    final def dynamic[A](expression: Option[A])(implicit ticket: CreationTicket): Event[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[A, S, Dynamic, rescala.reactives.Events.type]
  }

  implicit def EventOps[T](e: Event[T]): Events.EOps[T] = new Events.EOps[T](e)
  implicit def EventSeqOps[T](e: => Seq[Event[T]]): Events.ESeqOps[T] = new Events.ESeqOps[T](e)


  /** Contains static methods to create Events
    * @group create */
  object Events extends reactives.Events[S] {
    override val rescalaAPI: RescalaInterface.this.type = RescalaInterface.this
  }
  /** Contains static methods to create Signals
    * @group create */
  object Signals extends reactives.Signals[S] {
    override val rescalaAPI: RescalaInterface.this.type = RescalaInterface.this
  }

  object Impls extends DefaultImplementations[S] {
    override val rescalaAPI: RescalaInterface.this.type = RescalaInterface.this
  }

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
    scheduler.forceNewTransaction(initialWrites: _*)(admissionPhase)
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
  def transactionWithWrapup[I, R](initialWrites: ReSource*)(admissionPhase: AdmissionTicket => I)(wrapUpPhase: (I, AccessTicket) => R): R = {
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
    scheduler.forceNewTransaction(changes.foldLeft(Set.empty[ReSource]) { case (accu, (source, _)) =>
      assert(!accu.contains(source), s"must not admit multiple values for the same source ($source was assigned multiple times)")
      accu + source
    }, { t =>
      for(change <- changes) admit(t, change)
    })
  }
  private def admit[A](t: AdmissionTicket, change: (Source[S, A], A)): Unit = change._1.admit(change._2)(t)
}
