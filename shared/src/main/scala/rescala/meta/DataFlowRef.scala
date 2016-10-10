package rescala.meta

import rescala.graph.Struct
import rescala.reactives.Signals

trait DataFlowRef[+T] {
  protected[meta] var graph : DataFlowGraph
  def node : Option[DataFlowNode[T]]

  def disconnect() = node.get.disconnect()
}

object DataFlowRef {
  def unapply[T](arg: DataFlowRef[T]): Option[DataFlowNode[T]] = arg.node
}

trait ReactiveRef[+T] extends DataFlowRef[T] {
  override def node : Option[ReactiveNode[T]]

  def observe[U >: T](onSuccess: (U) => Unit, onFailure: (Throwable) => Unit = t => throw t): ObserveRef[U] = new ObserveRef(node.get.observe(onSuccess, onFailure))
  def reify[S <: Struct](reifier: Reifier[S]) = node.get.reify(reifier)
}

object ReactiveRef {
  def unapply[T](arg: ReactiveRef[T]): Option[ReactiveNode[T]] = arg.node
}

class EventRef[+T](_node : EventNode[T]) extends ReactiveRef[T] {
  override protected[meta] var graph : DataFlowGraph = _node.graph
  graph.registerPointer(this, _node)

  override def node: Option[EventNode[T]] = graph.resolvePointer(this).asInstanceOf[Option[EventNode[T]]]

  override def reify[S <: Struct](reifier: Reifier[S]) = node.get.reify(reifier)

  def +=[X >: T](react: X => Unit): ObserveRef[X] = new ObserveRef(node.get += react)
  def ||[U >: T](others: EventRef[U]*): EventRef[U] = new EventRef(node.get||(others.map(_.node.get):_*))
  def &&[U >: T](pred: (U) => Boolean): EventRef[U] = new EventRef(node.get && pred)
  def \[U](other: EventRef[U]): EventRef[T] = new EventRef(node.get \ other.node.get)
  def and[X >: T, U, R](other: EventRef[U])(merger: (X, U) => R): EventRef[R] = new EventRef(node.get.and(other.node.get)(merger))
  def zip[U](other: EventRef[U]): EventRef[(T, U)] = new EventRef(node.get.zip(other.node.get))
  def map[X >: T, U](mapping: (X) => U): EventRef[U] = new EventRef(node.get.map(mapping))
  def fold[X >: T, A](init: A)(fold: (A, X) => A): SignalRef[A] = new SignalRef(node.get.fold(init)(fold))
  def toggle[A](a: SignalRef[A], b: SignalRef[A]): SignalRef[A] = new SignalRef(node.get.toggle(a.node.get, b.node.get))
  def snapshot[A](s: SignalRef[A]): SignalRef[A] = new SignalRef(node.get.snapshot(s.node.get))
  def switchOnce[A](original: SignalRef[A], newSignal: SignalRef[A]): SignalRef[A] = new SignalRef(node.get.switchOnce(original.node.get, newSignal.node.get))
  def switchTo[A >: T](original: SignalRef[A]): SignalRef[A] = new SignalRef(node.get.switchTo(original.node.get))
  def flatMap[X >: T, B](f: (X) => EventRef[B]): EventRef[B] = new EventRef(node.get.flatMap({ x : X => f(x).node.get}))
}

object EventRef {
  def unapply[T](arg: EventRef[T]): Option[EventNode[T]] = arg.node
}

class EvtRef[T](__node : EvtEventNode[T]) extends EventRef(__node) {
  override def node: Option[EvtEventNode[T]] = graph.resolvePointer(this).asInstanceOf[Option[EvtEventNode[T]]]

  override def reify[S <: Struct](reifier: Reifier[S]) = node.get.reify(reifier)

  def fire(value: T) = node.get.fire(value)
}

object EvtRef {
  def unapply[T](arg: EvtRef[T]): Option[EvtEventNode[T]] = arg.node
}

class SignalRef[+A](_node : SignalNode[A]) extends ReactiveRef[A] {
  override protected[meta] var graph : DataFlowGraph = _node.graph
  graph.registerPointer(this, _node)

  override def node: Option[SignalNode[A]] = graph.resolvePointer(this).asInstanceOf[Option[SignalNode[A]]]

  override def reify[S <: Struct](reifier: Reifier[S]) = node.get.reify(reifier)

  def delay(n: Int): SignalRef[A] = new SignalRef(node.get.delay(n))
  def map[X >: A, B](f: (X) => B): SignalRef[B] = new SignalRef(node.get.map(f))
  def change: EventRef[Signals.Diff[A]] = new EventRef(node.get.change)
  def changed: EventRef[A] = new EventRef(node.get.changed)
}

object SignalRef {
  def unapply[T](arg: SignalRef[T]): Option[SignalNode[T]] = arg.node
}

class VarRef[A](__node : VarSignalNode[A]) extends SignalRef(__node) {
  override def node: Option[VarSignalNode[A]] = graph.resolvePointer(this).asInstanceOf[Option[VarSignalNode[A]]]

  override def reify[S <: Struct](reifier: Reifier[S]) = node.get.reify(reifier)

  def set(value: A) = node.get.set(value)
}

object VarRef {
  def unapply[T](arg: VarRef[T]): Option[VarSignalNode[T]] = arg.node
}

class ObserveRef[T](_node : ObserveNode[T]) extends DataFlowRef[Unit] {
  override protected[meta] var graph : DataFlowGraph = _node.graph
  graph.registerPointer(this, _node)

  override def node: Option[ObserveNode[T]] = graph.resolvePointer(this).asInstanceOf[Option[ObserveNode[T]]]

  def reify[S <: Struct](reifier: Reifier[S]) = node.get.reify(reifier)
}

object ObserveRef {
  def unapply[T](arg: ObserveRef[T]): Option[ObserveNode[T]] = arg.node
}
