package rescala.core

import rescala.reactives.Signal

import scala.annotation.implicitNotFound

/**
  * Propagation engine that defines the basic data-types available to the user and creates turns for propagation handling
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
@implicitNotFound(msg = "Could not find an implicit propagation engine. Did you forget an import?")
trait Scheduler[S <: Struct] {
  final def forceNewTransaction[R](initialWrites: ReSource[S]*)(admissionPhase: AdmissionTicket[S] => R): R = {
    forceNewTransaction(initialWrites.toSet, admissionPhase)
  }
  def forceNewTransaction[R](initialWrites: Set[ReSource[S]], admissionPhase: AdmissionTicket[S] => R): R
  private[rescala] def singleReadValueOnce[A](reactive: Signal[A, S]): A
  private[rescala] def initializerDynamicLookup[T](f: Initializer[S] => T): T

  /** Name of the scheduler, used for helpful error messages. */
  def schedulerName: String
  override def toString: String = s"Scheduler($schedulerName)"
}

object Scheduler {
  def apply[S <: Struct](implicit scheduler: Scheduler[S]): Scheduler[S] = scheduler
}
