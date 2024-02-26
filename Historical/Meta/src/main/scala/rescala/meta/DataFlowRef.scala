package reactives.meta

import reactives.core.{CreationTicket, Engine, Struct}

import reactives.reactives._

trait DataFlowRef[+T] {
  protected[meta] var graph: DataFlowGraph
  def deref: DataFlowNode[T]
  def tryDeref: Option[DataFlowNode[T]]

  def disconnect(): Unit = tryDeref.foreach(_.disconnect())

  def structuralEquals(dataFlowNode: DataFlowNode[_]): Boolean =
    tryDeref match {
      case Some(n) => n.structuralEquals(dataFlowNode)
      case None    => false
    }
  def structuralEquals(dataFlowRef: DataFlowRef[_]): Boolean =
    (tryDeref, dataFlowRef.tryDeref) match {
      case (Some(n1), Some(n2)) => n1.structuralEquals(n2)
      case (None, None)         => true
      case _                    => false
    }
}

object DataFlowRef {
  def unapply[T](arg: DataFlowRef[T]): Option[DataFlowNode[T]] = arg.tryDeref
}

trait ReactiveRef[+T] extends DataFlowRef[T] {
  override def tryDeref: Option[ReactiveNode[T]]
  override def deref: ReactiveNode[T] =
    tryDeref.getOrElse(throw new IllegalStateException("Trying to call operation on undefined reference!"))

  def observe[S <: Struct](onSuccess: (T) => Unit, onFailure: (Throwable) => Unit = t => throw t)(implicit
      ticket: CreationTicket[S]
  ): Unit =
    deref.observe(onSuccess, onFailure)
  def reify[S <: Struct](implicit reifier: Reifier[S]): Observable[T, S] = deref.reify
}

object ReactiveRef {
  def unapply[T](arg: ReactiveRef[T]): Option[ReactiveNode[T]] = arg.tryDeref
}

class EventRef[+T](_node: EventNode[T]) extends ReactiveRef[T] {
  override protected[meta] var graph: DataFlowGraph = _node.graph
  graph.registerRef(this, _node)

  override def tryDeref: Option[EventNode[T]] = graph.deref(this).asInstanceOf[Option[EventNode[T]]]
  override def deref: EventNode[T]            = super.deref.asInstanceOf[EventNode[T]]

  override def reify[S <: Struct](implicit reifier: Reifier[S]): Event[T, S] = deref.reify

  def +=[S <: Struct](react: T => Unit)(implicit ticket: CreationTicket[S]): Unit = deref += react

  def ||[U >: T](others: EventRef[U]*): EventRef[U] = new EventRef(deref || (others.map(_.deref): _*))
  def &&[U >: T](pred: (U) => Boolean): EventRef[U] = new EventRef(deref && pred)
  def \[U](other: EventRef[U]): EventRef[T]         = new EventRef(deref \ other.deref)
  def and[X >: T, U, R](other: EventRef[U])(merger: (X, U) => R): EventRef[R] =
    new EventRef(deref.and(other.deref)(merger))
  def zip[U](other: EventRef[U]): EventRef[(T, U)]              = new EventRef(deref.zip(other.deref))
  def map[X >: T, U](mapping: (X) => U): EventRef[U]            = new EventRef(deref.map(mapping))
  def fold[X >: T, A](init: A)(fold: (A, X) => A): SignalRef[A] = new SignalRef(deref.fold(init)(fold))
  def toggle[A](a: SignalRef[A], b: SignalRef[A]): SignalRef[A] = new SignalRef(deref.toggle(a.deref, b.deref))
  def snapshot[A](s: SignalRef[A]): SignalRef[A]                = new SignalRef(deref.snapshot(s.deref))
  def switchOnce[A](original: SignalRef[A], newSignal: SignalRef[A]): SignalRef[A] =
    new SignalRef(deref.switchOnce(original.deref, newSignal.deref))
  def switchTo[A >: T](original: SignalRef[A]): SignalRef[A] = new SignalRef(deref.switchTo(original.deref))
}

object EventRef {
  def unapply[T](arg: EventRef[T]): Option[EventNode[T]] = arg.tryDeref
}

class EvtRef[T](__node: EvtEventNode[T]) extends EventRef(__node) {
  override def tryDeref: Option[EvtEventNode[T]] = graph.deref(this).asInstanceOf[Option[EvtEventNode[T]]]
  override def deref: EvtEventNode[T]            = super.deref.asInstanceOf[EvtEventNode[T]]

  override def reify[S <: Struct](implicit reifier: Reifier[S]): Evt[T, S] = deref.reify

  def fire[S <: Struct](value: T)(implicit reifier: Reifier[S]): Unit = deref.fire(value)
}

object EvtRef {
  def unapply[T](arg: EvtRef[T]): Option[EvtEventNode[T]] = arg.tryDeref
}

class SignalRef[+A](_node: SignalNode[A]) extends ReactiveRef[A] {
  override protected[meta] var graph: DataFlowGraph = _node.graph
  graph.registerRef(this, _node)

  override def tryDeref: Option[SignalNode[A]] = graph.deref(this).asInstanceOf[Option[SignalNode[A]]]
  override def deref: SignalNode[A]            = super.deref.asInstanceOf[SignalNode[A]]

  override def reify[S <: Struct](implicit reifier: Reifier[S]): Signal[A, S] = deref.reify

  def now[S <: Struct](implicit reifier: Reifier[S], ticket: Engine[S]): A = deref.now

  def map[X >: A, B](f: (X) => B): SignalRef[B] = new SignalRef(deref.map(f))
  def change: EventRef[Signals.Diff[A]]         = new EventRef(deref.change)
  def changed: EventRef[A]                      = new EventRef(deref.changed)
}

object SignalRef {
  def unapply[T](arg: SignalRef[T]): Option[SignalNode[T]] = arg.tryDeref
}

class VarRef[A](__node: VarSignalNode[A]) extends SignalRef(__node) {
  override def tryDeref: Option[VarSignalNode[A]] = graph.deref(this).asInstanceOf[Option[VarSignalNode[A]]]
  override def deref: VarSignalNode[A]            = super.deref.asInstanceOf[VarSignalNode[A]]

  override def reify[S <: Struct](implicit reifier: Reifier[S]): Var[A, S] = deref.reify

  def set[S <: Struct](value: A)(implicit reifier: Reifier[S]): Unit = deref.set(value)
}

object VarRef {
  def unapply[T](arg: VarRef[T]): Option[VarSignalNode[T]] = arg.tryDeref
}
