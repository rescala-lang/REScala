package rescala.meta

import rescala.engines.Ticket
import rescala.graph.Struct
import rescala.reactives.{Observe, Signals}

trait DataFlowRef[+T] {
  protected[meta] var graph : DataFlowGraph
  def deref : Option[DataFlowNode[T]]

  def disconnect() = deref.get.disconnect()

  def structuralEquals(dataFlowNode: DataFlowNode[_]): Boolean = deref match {
    case Some(n) => n.structuralEquals(dataFlowNode)
    case None => false
  }
  def structuralEquals(dataFlowRef: DataFlowRef[_]): Boolean = (deref, dataFlowRef.deref) match {
    case (Some(n1), Some(n2)) => n1.structuralEquals(n2)
    case (None, None) => true
    case _ => false
  }
}

object DataFlowRef {
  def unapply[T](arg: DataFlowRef[T]): Option[DataFlowNode[T]] = arg.deref
}

trait ReactiveRef[+T] extends DataFlowRef[T] {
  override def deref : Option[ReactiveNode[T]]

  def observe[S <: Struct](onSuccess: (T) => Unit, onFailure: (Throwable) => Unit = t => throw t)(implicit reifier: Reifier[S], ticket: Ticket[S]): Observe[S] = deref.get.observe(onSuccess, onFailure)
  def reify[S <: Struct](implicit reifier: Reifier[S]) = deref.get.reify
}

object ReactiveRef {
  def unapply[T](arg: ReactiveRef[T]): Option[ReactiveNode[T]] = arg.deref
}

class EventRef[+T](_node : EventNode[T]) extends ReactiveRef[T] {
  override protected[meta] var graph : DataFlowGraph = _node.graph
  graph.registerRef(this, _node)

  override def deref: Option[EventNode[T]] = graph.deref(this).asInstanceOf[Option[EventNode[T]]]

  override def reify[S <: Struct](implicit reifier: Reifier[S]) = deref.get.reify

  def +=[S <: Struct](react: T => Unit)(implicit reifier: Reifier[S], ticket: Ticket[S]): Observe[S] = deref.get += react

  def ||[U >: T](others: EventRef[U]*): EventRef[U] = new EventRef(deref.get||(others.map(_.deref.get):_*))
  def &&[U >: T](pred: (U) => Boolean): EventRef[U] = new EventRef(deref.get && pred)
  def \[U](other: EventRef[U]): EventRef[T] = new EventRef(deref.get \ other.deref.get)
  def and[X >: T, U, R](other: EventRef[U])(merger: (X, U) => R): EventRef[R] = new EventRef(deref.get.and(other.deref.get)(merger))
  def zip[U](other: EventRef[U]): EventRef[(T, U)] = new EventRef(deref.get.zip(other.deref.get))
  def map[X >: T, U](mapping: (X) => U): EventRef[U] = new EventRef(deref.get.map(mapping))
  def fold[X >: T, A](init: A)(fold: (A, X) => A): SignalRef[A] = new SignalRef(deref.get.fold(init)(fold))
  def toggle[A](a: SignalRef[A], b: SignalRef[A]): SignalRef[A] = new SignalRef(deref.get.toggle(a.deref.get, b.deref.get))
  def snapshot[A](s: SignalRef[A]): SignalRef[A] = new SignalRef(deref.get.snapshot(s.deref.get))
  def switchOnce[A](original: SignalRef[A], newSignal: SignalRef[A]): SignalRef[A] = new SignalRef(deref.get.switchOnce(original.deref.get, newSignal.deref.get))
  def switchTo[A >: T](original: SignalRef[A]): SignalRef[A] = new SignalRef(deref.get.switchTo(original.deref.get))
  def flatMap[X >: T, B](f: (X) => EventRef[B]): EventRef[B] = new EventRef(deref.get.flatMap({ x : X => f(x).deref.get}))
}

object EventRef {
  def unapply[T](arg: EventRef[T]): Option[EventNode[T]] = arg.deref
}

class EvtRef[T](__node : EvtEventNode[T]) extends EventRef(__node) {
  override def deref: Option[EvtEventNode[T]] = graph.deref(this).asInstanceOf[Option[EvtEventNode[T]]]

  override def reify[S <: Struct](implicit reifier: Reifier[S]) = deref.get.reify

  def fire(value: T) = deref.get.fire(value)
}

object EvtRef {
  def unapply[T](arg: EvtRef[T]): Option[EvtEventNode[T]] = arg.deref
}

class SignalRef[+A](_node : SignalNode[A]) extends ReactiveRef[A] {
  override protected[meta] var graph : DataFlowGraph = _node.graph
  graph.registerRef(this, _node)

  override def deref: Option[SignalNode[A]] = graph.deref(this).asInstanceOf[Option[SignalNode[A]]]

  override def reify[S <: Struct](implicit reifier: Reifier[S]) = deref.get.reify

  def now[S <: Struct](implicit reifier: Reifier[S], ticket: Ticket[S]): A = deref.get.now

  def delay(n: Int): SignalRef[A] = new SignalRef(deref.get.delay(n))
  def map[X >: A, B](f: (X) => B): SignalRef[B] = new SignalRef(deref.get.map(f))
  def change: EventRef[Signals.Diff[A]] = new EventRef(deref.get.change)
  def changed: EventRef[A] = new EventRef(deref.get.changed)
}

object SignalRef {
  def unapply[T](arg: SignalRef[T]): Option[SignalNode[T]] = arg.deref
}

class VarRef[A](__node : VarSignalNode[A]) extends SignalRef(__node) {
  override def deref: Option[VarSignalNode[A]] = graph.deref(this).asInstanceOf[Option[VarSignalNode[A]]]

  override def reify[S <: Struct](implicit reifier: Reifier[S]) = deref.get.reify

  def set(value: A) = deref.get.set(value)
}

object VarRef {
  def unapply[T](arg: VarRef[T]): Option[VarSignalNode[T]] = arg.deref
}