package rescala.meta.reactives

import rescala.engines.Ticket
import rescala.graph.Struct
import rescala.propagation.Turn
import rescala.reactives.{Observe, Signal}

/**
  * Intermediate trait mainly used to satisfy the type requirements of the Signal/Event interfaces.
  *
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
trait ManagedSignal[+A, S <: Struct] extends Signal[A, S, ManagedSignal, ManagedEvent] {
}

/**
  * Actual implementation of a managed signal that has its propagation handled by a connected meta-graph representation.
  *
  * @tparam A Type stored by the signal
  */
class ManagedSignalImpl[+A] extends ManagedSignal[A, DummyStruct] {
  /** add an observer */
  override def observe(react: (A) => Unit)(implicit ticket: Ticket[DummyStruct]): Observe[DummyStruct] = ???

  /** Return a Signal with f applied to the value */
  override def map[B](f: (A) => B)(implicit ticket: Ticket[DummyStruct]): ManagedSignalImpl[B] = ???

  /** flatten the inner signal */
  override def flatten[B]()(implicit ev: <:<[A, ManagedSignal[B, DummyStruct]], ticket: Ticket[DummyStruct]): ManagedSignalImpl[B] = ???

  /** Unwraps a Signal[Event[EV, S], S] to an Event[EV, S] */
  override def unwrap[E](implicit evidence: <:<[A, ManagedEvent[E, DummyStruct]], ticket: Ticket[DummyStruct]): ManagedEventImpl[E] = ???

  /**
    * Create an event that fires every time the signal changes. It fires the tuple
    * (oldVal, newVal) for the signal. The first tuple is (null, newVal)
    */
  override def change(implicit ticket: Ticket[DummyStruct]): ManagedEventImpl[(A, A)] = ???

  override def apply[T](turn: Turn[DummyStruct]): A = ???

  override def now(implicit maybe: Ticket[DummyStruct]): A = ???

  override def get(implicit turn: Turn[DummyStruct]): A = ???
}
