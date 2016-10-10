package rescala.meta

import rescala.graph.Struct
import rescala.reactives.Signals

trait DataFlowPointer[+T] {
  protected[meta] var graph : DataFlowGraph
  def node : Option[DataFlowNode[T]]

  def disconnect() = node.get.disconnect()
}

trait ReactivePointer[+T] extends DataFlowPointer[T] {
  override def node : Option[ReactiveNode[T]]

  def observe[U >: T](onSuccess: (U) => Unit, onFailure: (Throwable) => Unit = t => throw t): ObservePointer[U] = new ObservePointer(node.get.observe(onSuccess, onFailure))
  def reify[S <: Struct](reifier: Reifier[S]) = node.get.reify(reifier)
}

class EventPointer[+T](_node : EventNode[T]) extends ReactivePointer[T] {
  override protected[meta] var graph : DataFlowGraph = _node.graph
  graph.registerPointer(this, _node)

  override def node: Option[EventNode[T]] = graph.resolvePointer(this) match {
    case Some(n) => Some(n.asInstanceOf[EventNode[T]])
    case None => None
  }

  override def reify[S <: Struct](reifier: Reifier[S]) = node.get.reify(reifier)

  def +=[X >: T](react: X => Unit): ObservePointer[X] = new ObservePointer(node.get += react)
  def ||[U >: T](others: EventPointer[U]*): EventPointer[U] = new EventPointer(node.get||(others.map(_.node.get):_*))
  def &&[U >: T](pred: (U) => Boolean): EventPointer[U] = new EventPointer(node.get && pred)
  def \[U](other: EventPointer[U]): EventPointer[T] = new EventPointer(node.get \ other.node.get)
  def and[X >: T, U, R](other: EventPointer[U])(merger: (X, U) => R): EventPointer[R] = new EventPointer(node.get.and(other.node.get)(merger))
  def zip[U](other: EventPointer[U]): EventPointer[(T, U)] = new EventPointer(node.get.zip(other.node.get))
  def map[X >: T, U](mapping: (X) => U): EventPointer[U] = new EventPointer(node.get.map(mapping))
  def fold[X >: T, A](init: A)(fold: (A, X) => A): SignalPointer[A] = new SignalPointer(node.get.fold(init)(fold))
  def toggle[A](a: SignalPointer[A], b: SignalPointer[A]): SignalPointer[A] = new SignalPointer(node.get.toggle(a.node.get, b.node.get))
  def snapshot[A](s: SignalPointer[A]): SignalPointer[A] = new SignalPointer(node.get.snapshot(s.node.get))
  def switchOnce[A](original: SignalPointer[A], newSignal: SignalPointer[A]): SignalPointer[A] = new SignalPointer(node.get.switchOnce(original.node.get, newSignal.node.get))
  def switchTo[A >: T](original: SignalPointer[A]): SignalPointer[A] = new SignalPointer(node.get.switchTo(original.node.get))
  def flatMap[X >: T, B](f: (X) => EventPointer[B]): EventPointer[B] = new EventPointer(node.get.flatMap({x : X => f(x).node.get}))
}

class EvtPointer[T](protected[this] var __node : EvtEventNode[T]) extends EventPointer(__node) {
  override def node: Option[EvtEventNode[T]] = graph.resolvePointer(this) match {
    case Some(n) => Some(n.asInstanceOf[EvtEventNode[T]])
    case None => None
  }

  override def reify[S <: Struct](reifier: Reifier[S]) = node.get.reify(reifier)

  def fire(value: T) = node.get.fire(value)

}

class SignalPointer[+A](_node : SignalNode[A]) extends ReactivePointer[A] {
  override protected[meta] var graph : DataFlowGraph = _node.graph
  graph.registerPointer(this, _node)

  override def node: Option[SignalNode[A]] = graph.resolvePointer(this) match {
    case Some(n) => Some(n.asInstanceOf[SignalNode[A]])
    case None => None
  }

  override def reify[S <: Struct](reifier: Reifier[S]) = node.get.reify(reifier)

  def delay(n: Int): SignalPointer[A] = new SignalPointer(node.get.delay(n))
  def map[X >: A, B](f: (X) => B): SignalPointer[B] = new SignalPointer(node.get.map(f))
  def change: EventPointer[Signals.Diff[A]] = new EventPointer(node.get.change)
  def changed: EventPointer[A] = new EventPointer(node.get.changed)
}

class VarPointer[A](__node : VarSignalNode[A]) extends SignalPointer(__node) {
  override def node: Option[VarSignalNode[A]] = graph.resolvePointer(this) match {
    case Some(n) => Some(n.asInstanceOf[VarSignalNode[A]])
    case None => None
  }

  override def reify[S <: Struct](reifier: Reifier[S]) = node.get.reify(reifier)

  def set(value: A) = node.get.set(value)
}

class ObservePointer[T](_node : ObserveNode[T]) extends DataFlowPointer[Unit] {
  override protected[meta] var graph : DataFlowGraph = _node.graph
  graph.registerPointer(this, _node)

  override def node: Option[ObserveNode[T]] = graph.resolvePointer(this) match {
    case Some(n) => Some(n.asInstanceOf[ObserveNode[T]])
    case None => None
  }

  def reify[S <: Struct](reifier: Reifier[S]) = node.get.reify(reifier)
}
