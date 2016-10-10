package rescala.meta

import rescala.graph.Struct
import rescala.reactives.Signals

trait DataFlowPointer[+T] {
  def node : DataFlowNode[T]

  def disconnect() = node.disconnect()
}

trait ReactivePointer[+T] extends DataFlowPointer[T] {
  override def node : ReactiveNode[T]

  def observe[U >: T](onSuccess: (U) => Unit, onFailure: (Throwable) => Unit = t => throw t): ObservePointer[U] = new ObservePointer(node.observe(onSuccess, onFailure))
  def reify[S <: Struct](reifier: Reifier[S]) = node.reify(reifier)
}

class EventPointer[+T](protected[this] var _node : EventNode[T]) extends ReactivePointer[T] {
  override def node: EventNode[T] = _node
  override def reify[S <: Struct](reifier: Reifier[S]) = node.reify(reifier)

  def +=[X >: T](react: X => Unit): ObservePointer[X] = new ObservePointer(node += react)
  def ||[U >: T](others: EventPointer[U]*): EventPointer[U] = new EventPointer(node||(others.map(_.node):_*))
  def &&[U >: T](pred: (U) => Boolean): EventPointer[U] = new EventPointer(node && pred)
  def \[U](other: EventPointer[U]): EventPointer[T] = new EventPointer(node \ other.node)
  def and[X >: T, U, R](other: EventPointer[U])(merger: (X, U) => R): EventPointer[R] = new EventPointer(node.and(other.node)(merger))
  def zip[U](other: EventPointer[U]): EventPointer[(T, U)] = new EventPointer(node.zip(other.node))
  def map[X >: T, U](mapping: (X) => U): EventPointer[U] = new EventPointer(node.map(mapping))
  def fold[X >: T, A](init: A)(fold: (A, X) => A): SignalPointer[A] = new SignalPointer(node.fold(init)(fold))
  def toggle[A](a: SignalPointer[A], b: SignalPointer[A]): SignalPointer[A] = new SignalPointer(node.toggle(a.node, b.node))
  def snapshot[A](s: SignalPointer[A]): SignalPointer[A] = new SignalPointer(node.snapshot(s.node))
  def switchOnce[A](original: SignalPointer[A], newSignal: SignalPointer[A]): SignalPointer[A] = new SignalPointer(node.switchOnce(original.node, newSignal.node))
  def switchTo[A >: T](original: SignalPointer[A]): SignalPointer[A] = new SignalPointer(node.switchTo(original.node))
  def flatMap[X >: T, B](f: (X) => EventPointer[B]): EventPointer[B] = new EventPointer(node.flatMap({x : X => f(x).node}))
}

class EvtPointer[T](protected[this] var __node : EvtEventNode[T]) extends EventPointer(__node) {
  override def node: EvtEventNode[T] = __node
  override def reify[S <: Struct](reifier: Reifier[S]) = node.reify(reifier)

  def fire(value: T) = node.fire(value)

}

class SignalPointer[+A](protected[this] var _node : SignalNode[A]) extends ReactivePointer[A] {
  override def node: SignalNode[A] = _node
  override def reify[S <: Struct](reifier: Reifier[S]) = node.reify(reifier)

  def delay(n: Int): SignalPointer[A] = new SignalPointer(node.delay(n))
  def map[X >: A, B](f: (X) => B): SignalPointer[B] = new SignalPointer(node.map(f))
  def change: EventPointer[Signals.Diff[A]] = new EventPointer(node.change)
  def changed: EventPointer[A] = new EventPointer(node.changed)
}

class VarPointer[T](protected[this] var __node : VarSignalNode[T]) extends SignalPointer(__node) {
  override def node: VarSignalNode[T] = __node
  override def reify[S <: Struct](reifier: Reifier[S]) = node.reify(reifier)

  def set(value: T) = node.set(value)
}

class ObservePointer[T](protected[this] var _node : ObserveNode[T]) extends DataFlowPointer[Unit] {
  override def node: ObserveNode[T] = _node
  def reify[S <: Struct](reifier: Reifier[S]) = node.reify(reifier)
}
