package rescala.interface

import rescala.core.ReName
import rescala.operator.{EventApi, Pulse, SignalApi, Sources}
import rescala.operator.DefaultImplementations

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
  */
trait RescalaInterface {
  self : EventApi with SignalApi with Sources with DefaultImplementations with RescalaInterface with Sources =>

  /** @group internal */
  def scheduler: Scheduler

  /** @group internal */
  implicit def implicitScheduler: Scheduler = scheduler

  /** @group create */
  final def Evt[A]()(implicit ticket: CreationTicket): Evt[A] = {
    ticket.createSource[Pulse[A], Evt[A]](Pulse.NoChange)(init =>
      {new Evt[A](init, ticket.rename)}: Evt[A]
    )
  }

  /** Creates new [[Var]]s
    * @group create
    */
  object Var {
    abstract class VarImpl[A] private[rescala] (initialState: State[Pulse[A]], name: ReName)
        extends Base[Pulse[A]](initialState, name)
        with Var[A]
        with Signals.SignalResource[A] {
      override val resource: Signals.SignalResource[A] = this
    }

    def apply[T](initval: T)(implicit ticket: CreationTicket): VarImpl[T] = fromChange(Pulse.Value(initval))
    def empty[T](implicit ticket: CreationTicket): VarImpl[T]             = fromChange(Pulse.empty)
    private[this] def fromChange[T](change: Pulse[T])(implicit ticket: CreationTicket): VarImpl[T] = {
      ticket.createSource[Pulse[T], VarImpl[T]](change)(s =>
        new VarImpl[T](s, ticket.rename) {
          override val resource                     = this
        }
      )
    }
  }


  implicit def OnEv[T](e: Event[T]): Events.OnEv[T]           = new Events.OnEv[T](e)
  implicit def OnEvs[T](e: => Seq[Event[T]]): Events.OnEvs[T] = new Events.OnEvs[T](e)

  /** Executes a transaction.
    *
    * @param initialWrites  All inputs that might be changed by the transaction
    * @param admissionPhase An admission function that may perform arbitrary [[rescala.operator.Signal.readValueOnce]] reads
    *                       to [[rescala.operator.Evt.admit]] / [[rescala.operator.Var.admit]] arbitrary
    *                       input changes that will be applied as an atomic transaction at the end.
    * @tparam R Result type of the admission function
    * @return Result of the admission function
    * @group update
    */
  def transaction[R](initialWrites: ReSource*)(admissionPhase: AdmissionTicket => R): R = {
    scheduler.forceNewTransaction(initialWrites: _*)(admissionPhase)
  }

  /** Executes a transaction with WrapUpPhase.
    *
    * @param initialWrites  All inputs that might be changed by the transaction
    * @param admissionPhase An admission function that may perform arbitrary [[rescala.operator.Signal.readValueOnce]] reads
    *                       to [[rescala.operator.Evt.admit]] / [[rescala.operator.Var.admit]] arbitrary
    *                       input changes that will be applied as an atomic transaction at the end.
    *                       The return value of this phase will be passed to the wrapUpPhase
    * @param wrapUpPhase    A wrap-up function that receives the admissionPhase result and may perform arbitrary
    *                       [[rescala.operator.Signal.readValueOnce]] reads which are
    *                       executed after the update propagation.
    * @tparam I Intermediate Result type passed from admission to wrapup phase
    * @tparam R Final Result type of the wrapup phase
    * @return Result of the wrapup function
    * @group update
    */
  def transactionWithWrapup[I, R](initialWrites: ReSource*)(admissionPhase: AdmissionTicket => I)(wrapUpPhase: (
      I,
      AccessTicket
  ) => R): R = {
    var res: Option[R] = None
    transaction(initialWrites: _*)(at => {
      val apr: I = admissionPhase(at)
      at.wrapUp = wut => { res = Some(wrapUpPhase(apr, wut)) }
    })
    res.get
  }
}
