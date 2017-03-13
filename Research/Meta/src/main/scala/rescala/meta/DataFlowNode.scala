package rescala.meta

import rescala.engine.{Engine, TurnSource}
import rescala.graph.{Pulse, Pulsing, Struct}
import rescala.propagation.Turn
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
  def newRef(): DataFlowRef[T]
  def dependencies : Set[DataFlowRef[_]]
  def structuralEquals(dataFlowNode: DataFlowNode[_]): Boolean

  def disconnect(): Unit = graph.addLog(LoggedDisconnect(this.newRef()))

  graph.registerNode(this)

}

trait ReactiveNode[+T] extends DataFlowNode[T] {
  protected[meta] var assignedReifier : Option[Reifier[_ <: Struct]] = None
  def hasReification: Boolean = assignedReifier.nonEmpty

  protected[this] def registerReifier(reifier: Reifier[_ <: Struct]): Unit = {
    if (assignedReifier.exists(_ != reifier))
      throw new IllegalAccessException("Cannot reify a node with different reifiers at the same time!")
    assignedReifier = Some(reifier)
  }
  def reify[S <: Struct](implicit reifier: Reifier[S]): Observable[T, S]
  def unreify[S <: Struct](implicit reifier: Reifier[S], ticket : TurnSource[S]): Unit = {
    reifier.unreify(this)
    assignedReifier = None
  }
  override def newRef(): ReactiveRef[T]

  protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Observable[T, S]
  protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Pulsing[Pulse[T], S]

  def observe[S <: Struct](onSuccess: (T) => Unit, onFailure: (Throwable) => Unit = t => throw t)(implicit reifier: Reifier[S], ticket: TurnSource[S]): Observe[S] = {
    reify.observe(onSuccess, onFailure)
  }
}

trait EventNode[+T] extends ReactiveNode[T] {
  override def reify[S <: Struct](implicit reifier: Reifier[S]): Event[T, S] = reifier.reifyEvent(this)
  override def newRef(): EventRef[T] = new EventRef(this)

  override protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Event[T, S] = {
    registerReifier(reifier)
    reifier.doReifyEvent(this)
  }
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Event[T, S]

  def +=[S <: Struct](react: T => Unit)(implicit reifier: Reifier[S], ticket: TurnSource[S]): Observe[S] = observe(react)

  def ||[U >: T](others: EventNode[U]*): OrEventNode[T, U] = OrEventNode(graph, newRef(), others.map(_.newRef()):_*)
  def &&[U >: T](pred: (U) => Boolean): FilteredEventNode[U, U] = FilteredEventNode[U, U](graph, newRef(), pred)
  def \[U](other: EventNode[U]): ExceptEventNode[T, U] = ExceptEventNode(graph, newRef(), other.newRef())
  def and[X >: T, U, R](other: EventNode[U])(merger: (X, U) => R): AndEventNode[X, U, R] = AndEventNode(graph, newRef(), other.newRef(), merger)
  def zip[U](other: EventNode[U]): ZippedEventNode[T, U] = ZippedEventNode(graph, newRef(), other.newRef())
  def map[X >: T, U](mapping: (X) => U): MappedEventNode[X, U] = MappedEventNode(graph, newRef(), mapping)
  def fold[X >: T, A](init: A)(fold: (A, X) => A): FoldedSignalNode[X, A] = FoldedSignalNode(graph, newRef(), init, fold)
  def toggle[A](a: SignalNode[A], b: SignalNode[A]): ToggledSignalNode[T, A] = ToggledSignalNode(graph, newRef(), a.newRef(), b.newRef())
  def snapshot[A](s: SignalNode[A]): SnapshotSignalNode[T, A] = SnapshotSignalNode(graph, newRef(), s.newRef())
  def switchOnce[A](original: SignalNode[A], newSignal: SignalNode[A]): SwitchOnceSignalNode[T, A] = SwitchOnceSignalNode(graph, newRef(), original.newRef(), newSignal.newRef())
  def switchTo[A >: T](original: SignalNode[A]): SwitchToSignalNode[T, A] = SwitchToSignalNode(graph, newRef(), original.newRef())
  def flatMap[X >: T, B](f: (X) => EventNode[B]): FlatMappedEventNode[X, B] = FlatMappedEventNode(graph, newRef(), { x => f(x).newRef() })
}

trait SignalNode[+A] extends ReactiveNode[A] {
  override def reify[S <: Struct](implicit reifier: Reifier[S]): Signal[A, S] = reifier.reifySignal(this)


  override def newRef(): SignalRef[A] = new SignalRef(this)

  override protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Signal[A, S] = {
    registerReifier(reifier)
    reifier.doReifySignal(this)
  }
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Signal[A, S]

  def now[S <: Struct](implicit reifier: Reifier[S], ticket: TurnSource[S]): A = reify.now

  def delay(n: Int): DelayedSignalNode[A] = DelayedSignalNode(graph, newRef(), n)
  def map[X >: A, B](f: (X) => B): MappedSignalNode[X, B] = MappedSignalNode(graph, newRef(), f)
  def change: ChangeEventNode[A] = ChangeEventNode(graph, newRef())
  def changed: ChangedEventNode[A] = ChangedEventNode(graph, newRef())
}

case class EvtEventNode[T](override var graph: DataFlowGraph) extends EventNode[T] {
  override def reify[S <: Struct](implicit reifier: Reifier[S]): Evt[T, S] = {
    registerReifier(reifier)
    reifier.reifyEvt(this)
  }

  override def newRef(): EvtRef[T] = new EvtRef(this)
  def structuralEquals(node: DataFlowNode[_]): Boolean = this == node

  override def dependencies: Set[DataFlowRef[_]] = Set()
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Evt[T, S] = reifier.createEvt()

  def fire(value : T) : Unit = {
    val log = LoggedFire(this.newRef(), value)
    assignedReifier match {
      case Some(reifier) => reifier.logOrApply(log)
      case _ => graph.addLog(log)
    }
  }
}
case class ChangeEventNode[+T](override var graph: DataFlowGraph, base : SignalRef[T]) extends EventNode[Signals.Diff[T]] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case ChangeEventNode(g, bs) => graph == g && bs.structuralEquals(base)
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Event[Signals.Diff[T], S] = base.deref.get.doReify(reifier).change
}
case class ChangedEventNode[+T](override var graph: DataFlowGraph, base : SignalRef[T]) extends EventNode[T] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case ChangedEventNode(g, b) => graph == g && b.structuralEquals(base)
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected [meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Event[T, S] = base.deref.get.doReify(reifier).changed
}
case class FilteredEventNode[T, +U >: T](override var graph: DataFlowGraph, base : EventRef[T], pred: (T) => Boolean) extends EventNode[U] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case FilteredEventNode(g, bs, p) => graph == g && bs.structuralEquals(base) && p == pred
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Event[T, S] = base.deref.get.doReify(reifier).filter(pred)
}
case class OrEventNode[+T <: U, +U](override var graph: DataFlowGraph, base : EventRef[T], others : EventRef[U]*) extends EventNode[U] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case OrEventNode(g, b, o@_*) => graph == g && others.size == o.size && (others :+ base).forall(n => (o :+ b).exists(n.structuralEquals(_)))
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base) ++ others
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Event[U, S] = others.foldLeft(base.deref.get.doReify(reifier) : Event[U, S])((acc, next) => acc || next.deref.get.doReify(reifier))
}
case class ExceptEventNode[+T, +U](override var graph: DataFlowGraph, base : EventRef[T], other : EventRef[U]) extends EventNode[T] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case ExceptEventNode(g, bs, o) => graph == g && bs.structuralEquals(base) && o.structuralEquals(other)
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, other)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Event[T, S] = base.deref.get.doReify(reifier) \ other.deref.get.doReify(reifier)
}
case class AndEventNode[T, U, +R](override var graph: DataFlowGraph, base : EventRef[T], other : EventRef[U], merger: (T, U) => R) extends EventNode[R] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case AndEventNode(g, b, o, m) => graph == g && b.structuralEquals(base) && o.structuralEquals(other) && m == merger
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, other)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Event[R, S] = base.deref.get.doReify(reifier).and(other.deref.get.doReify(reifier))(merger)
}
case class ZippedEventNode[+T, +U](override var graph: DataFlowGraph, base : EventRef[T], other : EventRef[U]) extends EventNode[(T, U)] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case ZippedEventNode(g, bs, o) => graph == g && bs.structuralEquals(base) && o.structuralEquals(other)
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, other)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Event[(T, U), S] = base.deref.get.doReify(reifier).zip(other.deref.get.doReify(reifier))
}
case class MappedEventNode[T, +U](override var graph: DataFlowGraph, base : EventRef[T], mapping: (T) => U) extends EventNode[U] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case MappedEventNode(g, bs, m) => graph == g && bs.structuralEquals(base) && m == mapping
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Event[U, S] = base.deref.get.doReify(reifier).map(mapping)
}
case class FlatMappedEventNode[T, +B](override var graph: DataFlowGraph, base : EventRef[T], f: (T) => EventRef[B]) extends EventNode[B] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case FlatMappedEventNode(g, bs, m) => graph == g && bs.structuralEquals(base) && f == m
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Event[B, S] = base.deref.get.doReify(reifier).flatMap(f(_).deref.get.doReify(reifier))
}

case class VarSignalNode[A](override var graph: DataFlowGraph) extends SignalNode[A] {
  override def reify[S <: Struct](implicit reifier: Reifier[S]): Var[A, S] = {
    registerReifier(reifier)
    reifier.reifyVar(this)
  }
  override def newRef(): VarRef[A] = new VarRef(this)
  def structuralEquals(node: DataFlowNode[_]): Boolean = this == node

  override def dependencies: Set[DataFlowRef[_]] = Set()
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Var[A, S] = reifier.createVar()

  def set(value : A) : Unit = {
    val log = LoggedSet(this.newRef(), value)
    assignedReifier match {
      case Some(reifier) => reifier.logOrApply(log)
      case _ => graph.addLog(log)
    }
  }
}
case class FoldedSignalNode[T, A](override var graph: DataFlowGraph, base : EventRef[T], init: A, fold: (A, T) => A) extends SignalNode[A] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case FoldedSignalNode(g, bs, i, f) => graph == g && bs.structuralEquals(base) && i == init && f == fold
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Signal[A, S] = base.deref.get.doReify(reifier).fold(init)(fold)
}
case class ToggledSignalNode[+T, +A](override var graph: DataFlowGraph, base : EventRef[T], a : SignalRef[A], b : SignalRef[A]) extends SignalNode[A] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case ToggledSignalNode(g, bs, ao, bo) => graph == g && bs.structuralEquals(base) && a.structuralEquals(ao) && b.structuralEquals(bo)
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, a, b)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Signal[A, S] = base.deref.get.doReify(reifier).toggle(a.deref.get.doReify(reifier), b.deref.get.doReify(reifier))
}
case class SnapshotSignalNode[+T, +A](override var graph: DataFlowGraph, base : EventRef[T], s : SignalRef[A]) extends SignalNode[A] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case SnapshotSignalNode(g, bs, so) => graph == g && bs.structuralEquals(base) && s.structuralEquals(so)
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, s)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Signal[A, S] = base.deref.get.doReify(reifier).snapshot(s.deref.get.doReify(reifier))
}
case class SwitchOnceSignalNode[+T, +A](override var graph: DataFlowGraph, base : EventRef[T], original : SignalRef[A], newSignal : SignalRef[A]) extends SignalNode[A] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case ToggledSignalNode(g, bs, o, n) => graph == g && bs.structuralEquals(base) && o.structuralEquals(original) && n.structuralEquals(newSignal)
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, original, newSignal)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Signal[A, S] = base.deref.get.doReify(reifier).switchOnce(original.deref.get.doReify(reifier), newSignal.deref.get.doReify(reifier))
}
case class SwitchToSignalNode[+T <: A, +A](override var graph: DataFlowGraph, base : EventRef[T], original : SignalRef[A]) extends SignalNode[A] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case SwitchToSignalNode(g, bs, o) => graph == g && bs.structuralEquals(base) && o.structuralEquals(original)
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, original)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Signal[A, S] = base.deref.get.doReify(reifier).switchTo(original.deref.get.doReify(reifier))
}
case class DelayedSignalNode[+A](override var graph: DataFlowGraph, base : SignalRef[A], n: Int) extends SignalNode[A] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case DelayedSignalNode(g, bs, no) => graph == g && bs.structuralEquals(base) && n == no
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Signal[A, S] = base.deref.get.doReify(reifier).delay(n)
}
case class MappedSignalNode[A, +U](override var graph: DataFlowGraph, base : SignalRef[A], mapping: (A) => U) extends SignalNode[U] {
  def structuralEquals(node: DataFlowNode[_]): Boolean = node match {
    case MappedSignalNode(g, b, m) => graph == g && b.structuralEquals(base) && m == mapping
    case _ => false
  }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: TurnSource[S]): Signal[U, S] = base.deref.get.doReify(reifier).map(mapping)
}
