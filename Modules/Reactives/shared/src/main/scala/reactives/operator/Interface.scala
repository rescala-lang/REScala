package reactives.operator

import reactives.core.{AdmissionTicket, ReSource, Scheduler, Transaction}
import reactives.scheduler.LevelbasedVariants
import reactives.operator.Interface.State

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
trait Interface {

  export reactives.operator.{Signal, Event, Var, Evt, Fold, Flatten}

  // TODO: remove once no more references are present
  type BundleState[V] = State[V]

  /** @group internal */
  val scheduler: Scheduler[State]

  /** @group internal */
  implicit def implicitScheduler: Scheduler[State] = scheduler

  override def toString: String = s"Api»${scheduler.schedulerName}«"

  /** Executes a transaction.
    *
    * @param initialWrites  All inputs that might be changed by the transaction
    * @param admissionPhase An admission function that may
    *                       [[reactives.operator.Evt.admit]] / [[reactives.operator.Var.admit]] arbitrary
    *                       input changes that will be applied as an atomic transaction at the end.
    * @tparam R Result type of the admission function
    * @return Result of the admission function
    * @group update
    * @example transaction(a, b){ implicit at => a.set(5); b.set(1); at.now(a) }
    */
  def transaction[R](initialWrites: ReSource.of[State]*)(admissionPhase: AdmissionTicket[State] => R): R = {
    scheduler.forceNewTransaction(initialWrites*)(admissionPhase)
  }

  /** Executes a transaction with WrapUpPhase.
    * @see transaction
    * @group update
    */
  def transactionWithWrapup[I, R](iw: ReSource.of[State]*)(ap: AdmissionTicket[State] => I)(wrapUp: (
      I,
      Transaction[State]
  ) => R): R = {
    var res: Option[R] = None
    transaction(iw*)(at => {
      val apr: I = ap(at)
      at.wrapUp = wut => { res = Some(wrapUp(apr, wut)) }
    })
    res.get
  }
}

object Interface {
  class FromScheduler(override val scheduler: Scheduler[State]) extends Interface
  def from(sched: Scheduler[State]): FromScheduler =
    FromScheduler(sched)

  type State[V] = LevelbasedVariants.State[V]
  val default: Scheduler[State] = LevelbasedVariants.synchron

}
