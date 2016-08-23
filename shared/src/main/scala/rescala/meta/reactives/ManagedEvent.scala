package rescala.meta.reactives

import rescala.engines.{Engine, Ticket}
import rescala.graph.Struct
import rescala.meta.{ManagedReactive, ReactiveNode}
import rescala.propagation.Turn
import rescala.reactives.{EventLike, Evt, EvtLike, Observe}

import scala.util.Try

/**
  * Intermediate trait mainly used to satisfy the type requirements of the Signal/Event interfaces.
  *
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  */
trait ManagedEvent[+T, S <: Struct] extends EventLike[T, S, ManagedSignal, ManagedEvent] with ManagedReactive[T]

/**
  * Actual implementation of a managed event that has its propagation handled by a connected meta-graph representation.
  *
  * @tparam T Type returned when the event fires
  */
class ManagedEventImpl[+T](override val node: ReactiveNode) extends ManagedEvent[T, DummyStruct] {


  override def observe(onSuccess: (T) => Unit, onFailure: (Throwable) => Unit)(implicit ticket: Ticket[DummyStruct]): Observe[DummyStruct] = ???
  /**
    * Events disjunction.
    */
  override def ||[U >: T](other: ManagedEvent[U, DummyStruct])(implicit ticket: Ticket[DummyStruct]): ManagedEventImpl[U] = ???

  /**
    * Event filtered with a predicate
    */
  override def filter(pred: (T) => Boolean)(implicit ticket: Ticket[DummyStruct]): ManagedEventImpl[T] = ???

  /**
    * Event is triggered except if the other one is triggered
    */
  override def \[U](other: ManagedEvent[U, DummyStruct])(implicit ticket: Ticket[DummyStruct]): ManagedEventImpl[T] = ???

  /**
    * Events conjunction
    */
  override def and[U, R](other: ManagedEvent[U, DummyStruct])(merger: (T, U) => R)(implicit ticket: Ticket[DummyStruct]): ManagedEventImpl[R] = ???

  /**
    * Event conjunction with a merge method creating a tuple of both event parameters
    */
  override def zip[U](other: ManagedEvent[U, DummyStruct])(implicit ticket: Ticket[DummyStruct]): ManagedEventImpl[(T, U)] = ???

  /**
    * Transform the event parameter
    */
  override def map[U](mapping: (T) => U)(implicit ticket: Ticket[DummyStruct]): ManagedEventImpl[U] = ???

  /** folds events with a given fold function to create a Signal */
  override def fold[A](init: A)(fold: (A, T) => A)(implicit ticket: Ticket[DummyStruct]): ManagedSignalImpl[A] = ???

  /** Switch back and forth between two signals on occurrence of event e */
  override def toggle[A](a: ManagedSignal[A, DummyStruct], b: ManagedSignal[A, DummyStruct])(implicit ticket: Ticket[DummyStruct]): ManagedSignalImpl[A] = ???

  /** Return a Signal that is updated only when e fires, and has the value of the signal s */
  override def snapshot[A](s: ManagedSignal[A, DummyStruct])(implicit ticket: Ticket[DummyStruct]): ManagedSignalImpl[A] = ???

  /** Switch to a new Signal once, on the occurrence of event e. */
  override def switchOnce[A](original: ManagedSignal[A, DummyStruct], newSignal: ManagedSignal[A, DummyStruct])(implicit ticket: Ticket[DummyStruct]): ManagedSignalImpl[A] = ???

  /**
    * Switch to a signal once, on the occurrence of event e. Initially the
    * return value is set to the original signal. When the event fires,
    * the result is a constant signal whose value is the value of the event.
    */
  override def switchTo[T1 >: T](original: ManagedSignal[T1, DummyStruct])(implicit ticket: Ticket[DummyStruct]): ManagedSignalImpl[T1] = ???

  /** returns the values produced by the last event produced by mapping this value */
  override def flatMap[B](f: (T) => ManagedEvent[B, DummyStruct])(implicit ticket: Ticket[DummyStruct]): ManagedEventImpl[B] = ???

  override def lazyFold[A](init: => A)(folder: (=> A, T) => A)(implicit ticket: Ticket[DummyStruct]): ManagedSignalImpl[A] = ???
}

trait ManagedEvt[T, S <: Struct] extends ManagedEvent[T, S] with EvtLike[T, S, ManagedSignal, ManagedEvent]

class ManagedEvtImpl[T](override val node: ReactiveNode) extends ManagedEventImpl[T](node) with ManagedEvt[T, DummyStruct] {
  override def fire(value: T)(implicit fac: Engine[DummyStruct, Turn[DummyStruct]]): Unit = ???

}
