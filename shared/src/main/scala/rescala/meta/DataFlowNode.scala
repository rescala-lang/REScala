package rescala.meta

import rescala.engines.Ticket
import rescala.graph.{Pulsing, Struct}
import rescala.reactives._

trait DataFlowNode[+T] {
  // Used only to prevent structural equality check for case-classes
  private class Node
  private val _node = new Node()
  protected[meta] var _hasReification = false
  def hasReification = _hasReification

  override def equals(obj: scala.Any): Boolean = obj match {
    case n: DataFlowNode[_] => n._node == _node
    case _ => false
  }
  override def hashCode(): Int = _node.hashCode()

  protected[meta] var graph : DataFlowGraph
  def createRef(): DataFlowRef[T]
  def dependencies : Set[DataFlowRef[_]]
  //def structuralEquals(dataFlowNode: DataFlowNode[_]): Boolean

  def disconnect() = graph.addLog(LoggedDisconnect(this.createRef()))

  graph.registerNode(this)

}

trait ReactiveNode[+T] extends DataFlowNode[T] {
  def reify[S <: Struct](reifier: Reifier[S]): Observable[T, S]
  override def createRef(): ReactiveRef[T]

  protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Observable[T, S]
  protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Pulsing[T, S]

  def observe[U >: T](onSuccess: (U) => Unit, onFailure: (Throwable) => Unit = t => throw t): ObserveNode[U] = ObserveNode(graph, createRef(), onSuccess, onFailure)
}

trait EventNode[+T] extends ReactiveNode[T] {
  override def reify[S <: Struct](reifier: Reifier[S]): Event[T, S] = reifier.reifyEvent(this)
  override def createRef(): EventRef[T] = new EventRef(this)

  override protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Event[T, S] = reifier.doReifyEvent(this)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S]

  def +=[X >: T](react: X => Unit): ObserveNode[X] = ObserveNode(graph, createRef(), react)
  def ||[U >: T](others: EventNode[U]*): OrEventNode[T, U] = OrEventNode(graph, createRef(), others.map(_.createRef()):_*)
  def &&[U >: T](pred: (U) => Boolean): FilteredEventNode[U, U] = FilteredEventNode[U, U](graph, createRef(), pred)
  def \[U](other: EventNode[U]): ExceptEventNode[T, U] = ExceptEventNode(graph, createRef(), other.createRef())
  def and[X >: T, U, R](other: EventNode[U])(merger: (X, U) => R): AndEventNode[X, U, R] = AndEventNode(graph, createRef(), other.createRef(), merger)
  def zip[U](other: EventNode[U]): ZippedEventNode[T, U] = ZippedEventNode(graph, createRef(), other.createRef())
  def map[X >: T, U](mapping: (X) => U): MappedEventNode[X, U] = MappedEventNode(graph, createRef(), mapping)
  def fold[X >: T, A](init: A)(fold: (A, X) => A): FoldedSignalNode[X, A] = FoldedSignalNode(graph, createRef(), init, fold)
  def toggle[A](a: SignalNode[A], b: SignalNode[A]): ToggledSignalNode[T, A] = ToggledSignalNode(graph, createRef(), a.createRef(), b.createRef())
  def snapshot[A](s: SignalNode[A]): SnapshotSignalNode[T, A] = SnapshotSignalNode(graph, createRef(), s.createRef())
  def switchOnce[A](original: SignalNode[A], newSignal: SignalNode[A]): SwitchOnceSignalNode[T, A] = SwitchOnceSignalNode(graph, createRef(), original.createRef(), newSignal.createRef())
  def switchTo[A >: T](original: SignalNode[A]): SwitchToSignalNode[T, A] = SwitchToSignalNode(graph, createRef(), original.createRef())
  def flatMap[X >: T, B](f: (X) => EventNode[B]): FlatMappedEventNode[X, B] = FlatMappedEventNode(graph, createRef(), { x => f(x).createRef() })
}

trait SignalNode[+A] extends ReactiveNode[A] {
  override def reify[S <: Struct](reifier: Reifier[S]): Signal[A, S] = reifier.reifySignal(this)
  override def createRef(): SignalRef[A] = new SignalRef(this)

  override protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Signal[A, S] = reifier.doReifySignal(this)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S]

  def delay(n: Int): DelayedSignalNode[A] = DelayedSignalNode(graph, createRef(), n)
  def map[X >: A, B](f: (X) => B): MappedSignalNode[X, B] = MappedSignalNode(graph, createRef(), f)
  def change: ChangeEventNode[A] = ChangeEventNode(graph, createRef())
  def changed: ChangedEventNode[A] = ChangedEventNode(graph, createRef())
}

case class ObserveNode[T](override var graph: DataFlowGraph, base : ReactiveRef[T], onSuccess: (T) => Unit, onFailure: (Throwable) => Unit = (cause) => { throw cause}) extends DataFlowNode[Unit] {
  def reify[S <: Struct](reifier: Reifier[S]): Observe[S] = reifier.reifyObserve(this)
  override def createRef(): ObserveRef[T] = new ObserveRef(this)

  protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Observe[S] = reifier.doReifyObserve(this)
  protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Observe[S] = base.node.get.doReify(reifier).observe(onSuccess, onFailure)

  override def dependencies: Set[DataFlowRef[_]] = Set(base)

}


case class EvtEventNode[T](override var graph: DataFlowGraph) extends EventNode[T] {
  override def reify[S <: Struct](reifier: Reifier[S]): Evt[T, S] = reifier.reifyEvt(this)
  override def createRef(): EvtRef[T] = new EvtRef(this)

  override def dependencies: Set[DataFlowRef[_]] = Set()
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Evt[T, S] = reifier.createEvt()

  def fire(value : T) : Unit = graph.addLog(LoggedFire(this.createRef(), value))
}
case class ChangeEventNode[+T](override var graph: DataFlowGraph, base : SignalRef[T]) extends EventNode[Signals.Diff[T]] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[Signals.Diff[T], S] = base.node.get.doReify(reifier).change
}
case class ChangedEventNode[+T](override var graph: DataFlowGraph, base : SignalRef[T]) extends EventNode[T] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected [meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.node.get.doReify(reifier).changed
}
case class FilteredEventNode[T, +U >: T](override var graph: DataFlowGraph, base : EventRef[T], pred: (T) => Boolean) extends EventNode[U] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.node.get.doReify(reifier).filter(pred)
}
case class OrEventNode[+T <: U, +U](override var graph: DataFlowGraph, base : EventRef[T], others : EventRef[U]*) extends EventNode[U] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base) ++ others
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[U, S] = others.foldLeft(base.node.get.doReify(reifier) : Event[U, S])((acc, next) => acc || next.node.get.doReify(reifier))
}
case class ExceptEventNode[+T, +U](override var graph: DataFlowGraph, base : EventRef[T], other : EventRef[U]) extends EventNode[T] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base, other)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.node.get.doReify(reifier) \ other.node.get.doReify(reifier)
}
case class AndEventNode[T, U, +R](override var graph: DataFlowGraph, base : EventRef[T], other : EventRef[U], merger: (T, U) => R) extends EventNode[R] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base, other)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[R, S] = base.node.get.doReify(reifier).and(other.node.get.doReify(reifier))(merger)
}
case class ZippedEventNode[+T, +U](override var graph: DataFlowGraph, base : EventRef[T], other : EventRef[U]) extends EventNode[(T, U)] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base, other)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[(T, U), S] = base.node.get.doReify(reifier).zip(other.node.get.doReify(reifier))
}
case class MappedEventNode[T, +U](override var graph: DataFlowGraph, base : EventRef[T], mapping: (T) => U) extends EventNode[U] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[U, S] = base.node.get.doReify(reifier).map(mapping)
}
case class FlatMappedEventNode[T, +B](override var graph: DataFlowGraph, base : EventRef[T], f: (T) => EventRef[B]) extends EventNode[B] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[B, S] = base.node.get.doReify(reifier).flatMap(f(_).node.get.doReify(reifier))
}

case class VarSignalNode[A](override var graph: DataFlowGraph) extends SignalNode[A] {
  override def reify[S <: Struct](reifier: Reifier[S]): Var[A, S] = reifier.reifyVar(this)
  override def createRef(): VarRef[A] = new VarRef(this)

  override def dependencies: Set[DataFlowRef[_]] = Set()
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Var[A, S] = reifier.createVar()

  def set(value : A) : Unit = graph.addLog(LoggedSet(this.createRef(), value))
}
case class FoldedSignalNode[T, A](override var graph: DataFlowGraph, base : EventRef[T], init: A, fold: (A, T) => A) extends SignalNode[A] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.node.get.doReify(reifier).fold(init)(fold)
}
case class ToggledSignalNode[+T, +A](override var graph: DataFlowGraph, base : EventRef[T], a : SignalRef[A], b : SignalRef[A]) extends SignalNode[A] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base, a, b)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.node.get.doReify(reifier).toggle(a.node.get.doReify(reifier), b.node.get.doReify(reifier))
}
case class SnapshotSignalNode[+T, +A](override var graph: DataFlowGraph, base : EventRef[T], s : SignalRef[A]) extends SignalNode[A] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base, s)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.node.get.doReify(reifier).snapshot(s.node.get.doReify(reifier))
}
case class SwitchOnceSignalNode[+T, +A](override var graph: DataFlowGraph, base : EventRef[T], original : SignalRef[A], newSignal : SignalRef[A]) extends SignalNode[A] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base, original, newSignal)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.node.get.doReify(reifier).switchOnce(original.node.get.doReify(reifier), newSignal.node.get.doReify(reifier))
}
case class SwitchToSignalNode[+T <: A, +A](override var graph: DataFlowGraph, base : EventRef[T], original : SignalRef[A]) extends SignalNode[A] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base, original)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.node.get.doReify(reifier).switchTo(original.node.get.doReify(reifier))
}
case class DelayedSignalNode[+A](override var graph: DataFlowGraph, base : SignalRef[A], n: Int) extends SignalNode[A] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.node.get.doReify(reifier).delay(n)
}
case class MappedSignalNode[A, +U](override var graph: DataFlowGraph, base : SignalRef[A], mapping: (A) => U) extends SignalNode[U] {
  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[U, S] = base.node.get.doReify(reifier).map(mapping)
}
