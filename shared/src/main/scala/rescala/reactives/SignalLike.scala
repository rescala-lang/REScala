package rescala.reactives

import java.util.concurrent.CompletionException

import rescala.engines.Ticket
import rescala.graph.{Pulse, Struct}

import scala.language.higherKinds

/**
  * Base signal interface for all signal implementations.
  * Please note that any signal implementation should have the SL type parameter set to itself and be paired with
  * exactly one event implementation it is compatible with by setting the EV type parameter.
  * This relationship needs to be symmetrical.
  *
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  * @tparam SL Signal type supported as parameter and used as return type for signal methods
  * @tparam EV Event type supported as parameter and used as return type for signal methods
  */
trait SignalLike[+A, S <: Struct, SL[+X, Z <: Struct] <: SignalLike[X, Z, SL, EV], EV[+X, Z <: Struct] <: EventLike[X, Z, SL, EV]] {
  this : SL[A, S] =>

  /** add an observer */
  def observe(
    onSuccess: A => Unit,
    onFailure: Throwable => Unit = t => throw new CompletionException("Unhandled exception on observe", t)
  )(implicit ticket: Ticket[S]): Observe[S]

  /** Return a Signal with f applied to the value */
  def map[B](f: A => B)(implicit ticket: Ticket[S]): SL[B, S]

  /** flatten the inner signal */
  def flatten[R](implicit ev: Flatten[A, S, R], ticket: Ticket[S]): R

  /** Delays this signal by n occurrences */
  def delay(n: Int)(implicit ticket: Ticket[S]): SL[A, S]

  /**
    * Create an event that fires every time the signal changes. It fires the tuple
    * (oldVal, newVal) for the signal. The first tuple is (null, newVal)
    */
  def change(implicit ticket: Ticket[S]): EV[Signals.Diff[A], S]

  /**
    * Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    */
  def changed(implicit ticket: Ticket[S]): EV[A, S]

  /** Convenience function filtering to events which change this reactive to value */
  final def changedTo[V](value: V)(implicit ticket: Ticket[S]): EV[Unit, S] = (changed filter {_ == value}).dropParam

}

