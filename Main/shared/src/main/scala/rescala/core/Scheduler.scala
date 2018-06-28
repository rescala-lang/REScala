package rescala.core

import rescala.reactives.Signal

import scala.annotation.implicitNotFound
import scala.util.DynamicVariable

/**
  * Propagation engine that defines the basic data-types available to the user and creates turns for propagation handling
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
@implicitNotFound(msg = "Could not find an implicit propagation engine. Did you forget an import?")
trait Scheduler[S <: Struct] {
  final def executeTurn[R](initialWrites: ReSource[S]*)(admissionPhase: AdmissionTicket[S] => R): R = {
    executeTurn(initialWrites.toSet, admissionPhase)
  }
  def executeTurn[R](initialWrites: Set[ReSource[S]], admissionPhase: AdmissionTicket[S] => R): R
  private[rescala] def singleReadValueOnce[A](reactive: Signal[A, S]): A
  private[rescala] def creationDynamicLookup[T](f: Initializer[S] => T): T

  /** Name of the scheduler, used for helpful error messages. */
  def schedulerName: String
  override def toString: String = s"Scheduler($schedulerName)"
}


trait SchedulerImpl[S <: Struct, ExactTurn <: Initializer[S]] extends Scheduler[S] {

  override private[rescala] def creationDynamicLookup[T](f: Initializer[S] => T) = {
    _currentTurn.value match {
      case Some(turn) => f(turn)
      case None => executeTurn(Set.empty, ticket => f(ticket.creation))
    }
  }

  protected val _currentTurn: DynamicVariable[Option[ExactTurn]] = new DynamicVariable[Option[ExactTurn]](None)
  private[rescala] def withTurn[R](turn: ExactTurn)(thunk: => R): R = _currentTurn.withValue(Some(turn))(thunk)
}

