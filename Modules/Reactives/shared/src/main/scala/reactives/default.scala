package reactives

import reactives.SelectedScheduler.State
import reactives.core.{AdmissionTicket, ReSource, Transaction}
import reactives.scheduler.{GlobalCandidate, LevelbasedVariants}

/** [[Event]] and [[Signal]] represent different time-changing values, commonly referred to as reactives.
  * Use [[Var]] to create signal sources and [[Evt]] to create event sources.
  *
  * Events and signals can be created from other reactives by using combinators,
  * signals additionally can be created using [[Signal]] expressions.
  */
object default {

  export reactives.operator.{Signal, Event, Var, Evt, Fold, Flatten}
  export Fold.current

  override def toString: String = s"Api»${SelectedScheduler.candidate.scheduler.schedulerName}«"

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
  def transaction[R](initialWrites: ReSource.of[State]*)(admissionPhase: AdmissionTicket[State] ?=> R): R = {
    SelectedScheduler.candidate.scheduler.forceNewTransaction(initialWrites*)(admissionPhase(using _))
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
    transaction(iw*)(at ?=> {
      val apr: I = ap(at)
      at.wrapUp = wut => { res = Some(wrapUp(apr, wut)) }
    })
    res.get
  }
}

object SelectedScheduler {
  val candidate: GlobalCandidate[GlobalCandidate.selected.State] = GlobalCandidate.selected

  type State[V] = reactives.scheduler.GlobalCandidate.selected.State[V]
}
