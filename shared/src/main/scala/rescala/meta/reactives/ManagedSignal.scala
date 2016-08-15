package rescala.meta.reactives

import rescala.engines.{Engine, Ticket}
import rescala.graph.Struct
import rescala.meta.{ManagedReactive, ReactiveNode}
import rescala.propagation.Turn
import rescala.reactives.{Observe, SignalLike, VarLike}

/**
  * Intermediate trait mainly used to satisfy the type requirements of the Signal/Event interfaces.
  *
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
trait ManagedSignal[+A, S <: Struct] extends SignalLike[A, S, ManagedSignal, ManagedEvent] with ManagedReactive

/**
  * Actual implementation of a managed signal that has its propagation handled by a connected meta-graph representation.
  *
  * @tparam A Type stored by the signal
  */
class ManagedSignalImpl[+A](override val node : ReactiveNode) extends ManagedSignal[A, DummyStruct] {


  /**
    * Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    */
  override def changed(implicit ticket: Ticket[DummyStruct]): ManagedEvent[A, DummyStruct] = ???
  /** Delays this signal by n occurrences */
  override def delay(n: Int)(implicit ticket: Ticket[DummyStruct]): ManagedSignal[A, DummyStruct] = ???
  /** add an observer */
  override def observe(onSuccess: (A) => Unit, onFailure: (Throwable) => Unit)(implicit ticket: Ticket[DummyStruct]): Observe[DummyStruct] = ???
  /** Return a Signal with f applied to the value */
  override def map[B](f: (A) => B)(implicit ticket: Ticket[DummyStruct]): ManagedSignalImpl[B] = ???

  /** flatten the inner signal */
  override def flatten[B](implicit ev: <:<[A, ManagedSignal[B, DummyStruct]], ticket: Ticket[DummyStruct]): ManagedSignalImpl[B] = ???

  /** Unwraps a Signal[Event[EV, S], S] to an Event[EV, S] */
  override def unwrap[E](implicit evidence: <:<[A, ManagedEvent[E, DummyStruct]], ticket: Ticket[DummyStruct]): ManagedEventImpl[E] = ???

  /**
    * Create an event that fires every time the signal changes. It fires the tuple
    * (oldVal, newVal) for the signal. The first tuple is (null, newVal)
    */
  override def change(implicit ticket: Ticket[DummyStruct]): ManagedEventImpl[(A, A)] = ???
}

trait ManagedVar[A, S <: Struct] extends ManagedSignal[A, S] with VarLike[A, S, ManagedSignal, ManagedEvent]

class ManagedVarImpl[A](override val node: ReactiveNode) extends ManagedSignalImpl[A](node) with ManagedVar[A, DummyStruct] {
  override def set(value: A)(implicit fac: Engine[DummyStruct, Turn[DummyStruct]]): Unit = ???

  override def transform(f: (A) => A)(implicit fac: Engine[DummyStruct, Turn[DummyStruct]]): Unit = ???

  override def admit(value: A)(implicit turn: Turn[DummyStruct]): Unit = ???
}
