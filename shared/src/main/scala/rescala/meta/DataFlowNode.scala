package rescala.meta

import rescala.engines.Ticket
import rescala.graph.{Pulsing, Struct}
import rescala.reactives._

trait DataFlowNode[+T] {
  // Used only to prevent structural equality check for case-classes
  private class Node
  private val _node = new Node()

  override def equals(obj: scala.Any): Boolean = obj match {
    case n: DataFlowNode[_] => n._node == _node
    case _ => false
  }
  override def hashCode(): Int = _node.hashCode()

  protected[meta] var graph : DataFlowGraph
  def dependencies : Set[DataFlowNode[_]]

  def disconnect() = graph.addLog(LoggedDisconnect(this))

  graph.registerNode(this)

}

trait ReactiveNode[+T] extends DataFlowNode[T] {
  def reify[S <: Struct](reifier: Reifier[S]): Observable[T, S]

  protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Observable[T, S]
  protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Pulsing[T, S]

  def observe[U >: T](onSuccess: (U) => Unit, onFailure: (Throwable) => Unit = t => throw t): ObserveNode[U] = ObserveNode(graph, this, onSuccess, onFailure)
}

trait EventNode[+T] extends ReactiveNode[T] {
  override def reify[S <: Struct](reifier: Reifier[S]): Event[T, S] = reifier.reifyEvent(this)
  override protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Event[T, S] = reifier.doReifyEvent(this)

  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S]

  def +=[X >: T](react: X => Unit): ObserveNode[X] = ObserveNode(graph, this, react)
  def ||[U >: T](others: EventNode[U]*): OrEventNode[T, U] = OrEventNode(graph, this, others:_*)
  def &&[U >: T](pred: (U) => Boolean): FilteredEventNode[U, U] = FilteredEventNode[U, U](graph, this, pred)
  def \[U](other: EventNode[U]): ExceptEventNode[T, U] = ExceptEventNode(graph, this, other)
  def and[X >: T, U, R](other: EventNode[U])(merger: (X, U) => R): AndEventNode[X, U, R] = AndEventNode(graph, this, other, merger)
  def zip[U](other: EventNode[U]): ZippedEventNode[T, U] = ZippedEventNode(graph, this, other)
  def map[X >: T, U](mapping: (X) => U): MappedEventNode[X, U] = MappedEventNode(graph, this, mapping)
  def fold[X >: T, A](init: A)(fold: (A, X) => A): FoldedSignalNode[X, A] = FoldedSignalNode(graph, this, init, fold)
  def toggle[A](a: SignalNode[A], b: SignalNode[A]): ToggledSignalNode[T, A] = ToggledSignalNode(graph, this, a, b)
  def snapshot[A](s: SignalNode[A]): SnapshotSignalNode[T, A] = SnapshotSignalNode(graph, this, s)
  def switchOnce[A](original: SignalNode[A], newSignal: SignalNode[A]): SwitchOnceSignalNode[T, A] = SwitchOnceSignalNode(graph, this, original, newSignal)
  def switchTo[A >: T](original: SignalNode[A]): SwitchToSignalNode[T, A] = SwitchToSignalNode(graph, this, original)
  def flatMap[X >: T, B](f: (X) => EventNode[B]): FlatMappedEventNode[X, B] = FlatMappedEventNode(graph, this, f)
}

trait SignalNode[+A] extends ReactiveNode[A] {
  override def reify[S <: Struct](reifier: Reifier[S]): Signal[A, S] = reifier.reifySignal(this)
  override protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Signal[A, S] = reifier.doReifySignal(this)

  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S]

  def delay(n: Int): DelayedSignalNode[A] = DelayedSignalNode(graph, this, n)
  def map[X >: A, B](f: (X) => B): MappedSignalNode[X, B] = MappedSignalNode(graph, this, f)
  def change: ChangeEventNode[A] = ChangeEventNode(graph, this)
  def changed: ChangedEventNode[A] = ChangedEventNode(graph, this)
}

case class ObserveNode[T](override var graph: DataFlowGraph, base : ReactiveNode[T], onSuccess: (T) => Unit, onFailure: (Throwable) => Unit = (cause) => { throw cause}) extends DataFlowNode[Unit] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base)

  def reify[S <: Struct](reifier: Reifier[S]): Observe[S] = reifier.reifyObserve(this)
  protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Observe[S] = reifier.doReifyObserve(this)

  protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Observe[S] = base.doReify(reifier).observe(onSuccess, onFailure)
}


case class EvtEventNode[T](override var graph: DataFlowGraph) extends EventNode[T] {
  override def reify[S <: Struct](reifier: Reifier[S]): Evt[T, S] = reifier.reifyEvt(this)

  override def dependencies: Set[DataFlowNode[_]] = Set()
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Evt[T, S] = reifier.createEvt()

  def fire(value : T) : Unit = graph.addLog(LoggedFire(this, value))
}
case class ChangeEventNode[+T](override var graph: DataFlowGraph, base : SignalNode[T]) extends EventNode[Signals.Diff[T]] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base)
  override def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[Signals.Diff[T], S] = base.doReify(reifier).change
}
case class ChangedEventNode[+T](override var graph: DataFlowGraph, base : SignalNode[T]) extends EventNode[T] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base)
  override protected [meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.doReify(reifier).changed
}
case class FilteredEventNode[T, +U >: T](override var graph: DataFlowGraph, base : EventNode[T], pred: (T) => Boolean) extends EventNode[U] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.doReify(reifier).filter(pred)
}
case class OrEventNode[+T <: U, +U](override var graph: DataFlowGraph, base : EventNode[T], others : EventNode[U]*) extends EventNode[U] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base) ++ others
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[U, S] = others.foldLeft(base.doReify(reifier) : Event[U, S])((acc, next) => acc || next.doReify(reifier))
}
case class ExceptEventNode[+T, +U](override var graph: DataFlowGraph, base : EventNode[T], other : EventNode[U]) extends EventNode[T] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base, other)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.doReify(reifier) \ other.doReify(reifier)
}
case class AndEventNode[T, U, +R](override var graph: DataFlowGraph, base : EventNode[T], other : EventNode[U], merger: (T, U) => R) extends EventNode[R] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base, other)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[R, S] = base.doReify(reifier).and(other.doReify(reifier))(merger)
}
case class ZippedEventNode[+T, +U](override var graph: DataFlowGraph, base : EventNode[T], other : EventNode[U]) extends EventNode[(T, U)] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base, other)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[(T, U), S] = base.doReify(reifier).zip(other.doReify(reifier))
}
case class MappedEventNode[T, +U](override var graph: DataFlowGraph, base : EventNode[T], mapping: (T) => U) extends EventNode[U] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[U, S] = base.doReify(reifier).map(mapping)
}
case class FlatMappedEventNode[T, +B](override var graph: DataFlowGraph, base : EventNode[T], f: (T) => EventNode[B]) extends EventNode[B] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[B, S] = base.doReify(reifier).flatMap(f(_).doReify(reifier))
}

case class VarSignalNode[A](override var graph: DataFlowGraph) extends SignalNode[A] {
  override def reify[S <: Struct](reifier: Reifier[S]): Var[A, S] = reifier.reifyVar(this)

  override def dependencies: Set[DataFlowNode[_]] = Set()
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Var[A, S] = reifier.createVar()

  def set(value : A) : Unit = graph.addLog(LoggedSet(this, value))
}
case class FoldedSignalNode[T, A](override var graph: DataFlowGraph, base : EventNode[T], init: A, fold: (A, T) => A) extends SignalNode[A] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.doReify(reifier).fold(init)(fold)
}
case class ToggledSignalNode[+T, +A](override var graph: DataFlowGraph, base : EventNode[T], a : SignalNode[A], b : SignalNode[A]) extends SignalNode[A] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base, a, b)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.doReify(reifier).toggle(a.doReify(reifier), b.doReify(reifier))
}
case class SnapshotSignalNode[+T, +A](override var graph: DataFlowGraph, base : EventNode[T], s : SignalNode[A]) extends SignalNode[A] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base, s)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.doReify(reifier).snapshot(s.doReify(reifier))
}
case class SwitchOnceSignalNode[+T, +A](override var graph: DataFlowGraph, base : EventNode[T], original : SignalNode[A], newSignal : SignalNode[A]) extends SignalNode[A] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base, original, newSignal)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.doReify(reifier).switchOnce(original.doReify(reifier), newSignal.doReify(reifier))
}
case class SwitchToSignalNode[+T <: A, +A](override var graph: DataFlowGraph, base : EventNode[T], original : SignalNode[A]) extends SignalNode[A] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base, original)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.doReify(reifier).switchTo(original.doReify(reifier))
}
case class DelayedSignalNode[+A](override var graph: DataFlowGraph, base : SignalNode[A], n: Int) extends SignalNode[A] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.doReify(reifier).delay(n)
}
case class MappedSignalNode[A, +U](override var graph: DataFlowGraph, base : SignalNode[A], mapping: (A) => U) extends SignalNode[U] {
  override def dependencies: Set[DataFlowNode[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[U, S] = base.doReify(reifier).map(mapping)
}
