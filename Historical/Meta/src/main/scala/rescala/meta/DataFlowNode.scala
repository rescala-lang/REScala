package reactives.meta

import reactives.core.{CreationTicket, Engine, Pulse, ReactiV, Struct}

import reactives.reactives._
import reactives.util.Globals

trait DataFlowNode[+T] {
  // Prevent structural equality check for case-classes
  private class Node
  private val _node = new Node()

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case n: DataFlowNode[_] => n._node == _node
      case _                  => false
    }
  override def hashCode(): Int = _node.hashCode()

  // Create nice names for debugging
  private val name: String      = Globals.declarationLocationName()
  override def toString: String = name

  protected[meta] var graph: DataFlowGraph
  def newRef(): DataFlowRef[T]
  def dependencies: Set[DataFlowRef[_]]
  def structuralEquals(dataFlowNode: DataFlowNode[_]): Boolean

  def reify[S <: Struct](implicit reifier: Reifier[S]): Any
  def unreify[S <: Struct](implicit reifier: Reifier[S], ticket: CreationTicket[S]): Unit
  def disconnect(): Unit = graph.addLog(LoggedDisconnect(this.newRef()))

  graph.registerNode(this)
}

trait ReactiveNode[+T] extends DataFlowNode[T] {
  protected[meta] var assignedReifier: Option[Reifier[_ <: Struct]] = None
  def hasReification: Boolean                                       = assignedReifier.nonEmpty

  protected[this] def registerReifier(reifier: Reifier[_ <: Struct]): Unit = {
    if (assignedReifier.exists(_ != reifier))
      throw new IllegalAccessException("Cannot reify a node with different reifiers at the same time!")
    assignedReifier = Some(reifier)
  }
  override def reify[S <: Struct](implicit reifier: Reifier[S]): Observable[T, S]
  override def unreify[S <: Struct](implicit reifier: Reifier[S], ticket: CreationTicket[S]): Unit = {
    reifier.unreify(this)
    assignedReifier = None
  }
  override def newRef(): ReactiveRef[T]

  protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Observable[T, S]
  protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): ReactiV[Pulse[T], S]

  def observe[S <: Struct](onSuccess: (T) => Unit, onFailure: (Throwable) => Unit = t => throw t)(implicit
      ticket: CreationTicket[S]
  ): Unit =
    graph.addLog(LoggedObserve(this.newRef(), ObserverData(onSuccess, onFailure, ticket)))
}

trait EventNode[+T] extends ReactiveNode[T] {
  override def reify[S <: Struct](implicit reifier: Reifier[S]): Event[T, S] = reifier.reifyEvent(this)
  override def newRef(): EventRef[T]                                         = new EventRef(this)

  override protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Event[T, S] = {
    reifier.doReifyEvent(this)
  }
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Event[T, S]

  def +=[S <: Struct](react: T => Unit)(implicit ticket: CreationTicket[S]): Unit = observe(react)

  def ||[U >: T](others: EventNode[U]*): OrEventNode[T, U] = OrEventNode(graph, newRef(), others.map(_.newRef()): _*)
  def &&[U >: T](pred: (U) => Boolean): FilteredEventNode[U, U] = FilteredEventNode[U, U](graph, newRef(), pred)
  def \[U](other: EventNode[U]): ExceptEventNode[T, U]          = ExceptEventNode(graph, newRef(), other.newRef())
  def and[X >: T, U, R](other: EventNode[U])(merger: (X, U) => R): AndEventNode[X, U, R] =
    AndEventNode(graph, newRef(), other.newRef(), merger)
  def zip[U](other: EventNode[U]): ZippedEventNode[T, U]       = ZippedEventNode(graph, newRef(), other.newRef())
  def map[X >: T, U](mapping: (X) => U): MappedEventNode[X, U] = MappedEventNode(graph, newRef(), mapping)
  def fold[X >: T, A](init: A)(fold: (A, X) => A): FoldedSignalNode[X, A] =
    FoldedSignalNode(graph, newRef(), init, fold)
  def toggle[A](a: SignalNode[A], b: SignalNode[A]): ToggledSignalNode[T, A] =
    ToggledSignalNode(graph, newRef(), a.newRef(), b.newRef())
  def snapshot[A](s: SignalNode[A]): SnapshotSignalNode[T, A] = SnapshotSignalNode(graph, newRef(), s.newRef())
  def switchOnce[A](original: SignalNode[A], newSignal: SignalNode[A]): SwitchOnceSignalNode[T, A] =
    SwitchOnceSignalNode(graph, newRef(), original.newRef(), newSignal.newRef())
  def switchTo[A >: T](original: SignalNode[A]): SwitchToSignalNode[T, A] =
    SwitchToSignalNode(graph, newRef(), original.newRef())
}

trait SignalNode[+A] extends ReactiveNode[A] {
  override def reify[S <: Struct](implicit reifier: Reifier[S]): Signal[A, S] = reifier.reifySignal(this)

  override def newRef(): SignalRef[A] = new SignalRef(this)

  override protected[meta] def doReify[S <: Struct](reifier: Reifier[S]): Signal[A, S] = {
    reifier.doReifySignal(this)
  }
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Signal[A, S]

  def now[S <: Struct](implicit reifier: Reifier[S], ticket: Engine[S]): A = reify.readValueOnce

  def map[X >: A, B](f: (X) => B): MappedSignalNode[X, B] = MappedSignalNode(graph, newRef(), f)
  def change: ChangeEventNode[A]                          = ChangeEventNode(graph, newRef())
  def changed: ChangedEventNode[A]                        = ChangedEventNode(graph, newRef())
}

case class EvtEventNode[T](override var graph: DataFlowGraph) extends EventNode[T] {
  override def reify[S <: Struct](implicit reifier: Reifier[S]): Evt[T, S] = {
    reifier.reifyEvt(this)
  }

  override def newRef(): EvtRef[T]                     = new EvtRef(this)
  def structuralEquals(node: DataFlowNode[_]): Boolean = this == node

  override def dependencies: Set[DataFlowRef[_]] = Set()
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Evt[T, S] = {
    registerReifier(reifier)
    reifier.createEvt()
  }

  def fire[S <: Struct](value: T)(implicit reifier: Reifier[S]): Unit = {
    graph.addLog(LoggedFire(this.newRef(), value))
    assignedReifier.getOrElse(reifier).evaluateNecessaryReification(graph)
  }
}
case class ChangeEventNode[+T](override var graph: DataFlowGraph, base: SignalRef[T])
    extends EventNode[Signals.Diff[T]] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case ChangeEventNode(g, bs) => graph == g && bs.structuralEquals(base)
      case _                      => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Event[Signals.Diff[T], S] = {
    registerReifier(reifier)
    base.deref.doReify(reifier).change
  }
}
case class ChangedEventNode[+T](override var graph: DataFlowGraph, base: SignalRef[T]) extends EventNode[T] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case ChangedEventNode(g, b) => graph == g && b.structuralEquals(base)
      case _                      => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Event[T, S] = {
    registerReifier(reifier)
    base.deref.doReify(reifier).changed
  }
}
case class FilteredEventNode[T, +U >: T](override var graph: DataFlowGraph, base: EventRef[T], pred: (T) => Boolean)
    extends EventNode[U] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case FilteredEventNode(g, bs, p) => graph == g && bs.structuralEquals(base) && p == pred
      case _                           => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Event[T, S] = {
    registerReifier(reifier)
    base.deref.doReify(reifier).filter(pred)
  }
}
case class OrEventNode[+T <: U, +U](override var graph: DataFlowGraph, base: EventRef[T], others: EventRef[U]*)
    extends EventNode[U] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case OrEventNode(g, b, o @ _*) =>
        graph == g && others.size == o.size && (others :+ base).forall(n => (o :+ b).exists(n.structuralEquals(_)))
      case _ => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base) ++ others
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Event[U, S] = {
    registerReifier(reifier)
    others.foldLeft(base.deref.doReify(reifier): Event[U, S])((acc, next) => acc || next.deref.doReify(reifier))
  }
}
case class ExceptEventNode[+T, +U](override var graph: DataFlowGraph, base: EventRef[T], other: EventRef[U])
    extends EventNode[T] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case ExceptEventNode(g, bs, o) => graph == g && bs.structuralEquals(base) && o.structuralEquals(other)
      case _                         => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, other)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Event[T, S] = {
    registerReifier(reifier)
    base.deref.doReify(reifier) \ other.deref.doReify(reifier)
  }
}
case class AndEventNode[T, U, +R](
    override var graph: DataFlowGraph,
    base: EventRef[T],
    other: EventRef[U],
    merger: (T, U) => R
) extends EventNode[R] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case AndEventNode(g, b, o, m) =>
        graph == g && b.structuralEquals(base) && o.structuralEquals(other) && m == merger
      case _ => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, other)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Event[R, S] = {
    registerReifier(reifier)
    base.deref.doReify(reifier).and(other.deref.doReify(reifier))(merger)
  }
}
case class ZippedEventNode[+T, +U](override var graph: DataFlowGraph, base: EventRef[T], other: EventRef[U])
    extends EventNode[(T, U)] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case ZippedEventNode(g, bs, o) => graph == g && bs.structuralEquals(base) && o.structuralEquals(other)
      case _                         => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, other)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Event[(T, U), S] = {
    registerReifier(reifier)
    base.deref.doReify(reifier).zip(other.deref.doReify(reifier))
  }
}
case class MappedEventNode[T, +U](override var graph: DataFlowGraph, base: EventRef[T], mapping: (T) => U)
    extends EventNode[U] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case MappedEventNode(g, bs, m) => graph == g && bs.structuralEquals(base) && m == mapping
      case _                         => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Event[U, S] = {
    registerReifier(reifier)
    base.deref.doReify(reifier).map(mapping)
  }
}

case class VarSignalNode[A](override var graph: DataFlowGraph) extends SignalNode[A] {
  override def reify[S <: Struct](implicit reifier: Reifier[S]): Var[A, S] = {
    registerReifier(reifier)
    reifier.reifyVar(this)
  }
  override def newRef(): VarRef[A]                     = new VarRef(this)
  def structuralEquals(node: DataFlowNode[_]): Boolean = this == node

  override def dependencies: Set[DataFlowRef[_]] = Set()
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Var[A, S] = {
    registerReifier(reifier)
    reifier.createVar()
  }

  def set[S <: Struct](value: A)(implicit reifier: Reifier[S]): Unit = {
    graph.addLog(LoggedSet(this.newRef(), value))
    assignedReifier.getOrElse(reifier).evaluateNecessaryReification(graph)
  }
}
case class FoldedSignalNode[T, A](override var graph: DataFlowGraph, base: EventRef[T], init: A, fold: (A, T) => A)
    extends SignalNode[A] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case FoldedSignalNode(g, bs, i, f) => graph == g && bs.structuralEquals(base) && i == init && f == fold
      case _                             => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Signal[A, S] = {
    registerReifier(reifier)
    base.deref.doReify(reifier).fold(init)(fold)
  }
}
case class ToggledSignalNode[+T, +A](
    override var graph: DataFlowGraph,
    base: EventRef[T],
    a: SignalRef[A],
    b: SignalRef[A]
) extends SignalNode[A] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case ToggledSignalNode(g, bs, ao, bo) =>
        graph == g && bs.structuralEquals(base) && a.structuralEquals(ao) && b.structuralEquals(bo)
      case _ => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, a, b)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Signal[A, S] = {
    registerReifier(reifier)
    base.deref.doReify(reifier).toggle(a.deref.doReify(reifier), b.deref.doReify(reifier))
  }
}
case class SnapshotSignalNode[+T, +A](override var graph: DataFlowGraph, base: EventRef[T], s: SignalRef[A])
    extends SignalNode[A] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case SnapshotSignalNode(g, bs, so) => graph == g && bs.structuralEquals(base) && s.structuralEquals(so)
      case _                             => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, s)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Signal[A, S] = {
    registerReifier(reifier)
    base.deref.doReify(reifier).snapshot(s.deref.doReify(reifier))
  }
}
case class SwitchOnceSignalNode[+T, +A](
    override var graph: DataFlowGraph,
    base: EventRef[T],
    original: SignalRef[A],
    newSignal: SignalRef[A]
) extends SignalNode[A] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case ToggledSignalNode(g, bs, o, n) =>
        graph == g && bs.structuralEquals(base) && o.structuralEquals(original) && n.structuralEquals(newSignal)
      case _ => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, original, newSignal)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Signal[A, S] = {
    registerReifier(reifier)
    base.deref.doReify(reifier).switchOnce(original.deref.doReify(reifier), newSignal.deref.doReify(reifier))
  }
}
case class SwitchToSignalNode[+T <: A, +A](override var graph: DataFlowGraph, base: EventRef[T], original: SignalRef[A])
    extends SignalNode[A] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case SwitchToSignalNode(g, bs, o) => graph == g && bs.structuralEquals(base) && o.structuralEquals(original)
      case _                            => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base, original)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Signal[A, S] = {
    registerReifier(reifier)
    base.deref.doReify(reifier).switchTo(original.deref.doReify(reifier))
  }
}
case class MappedSignalNode[A, +U](override var graph: DataFlowGraph, base: SignalRef[A], mapping: (A) => U)
    extends SignalNode[U] {
  def structuralEquals(node: DataFlowNode[_]): Boolean =
    node match {
      case MappedSignalNode(g, b, m) => graph == g && b.structuralEquals(base) && m == mapping
      case _                         => false
    }

  override def dependencies: Set[DataFlowRef[_]] = Set(base)
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit
      ticket: CreationTicket[S]
  ): Signal[U, S] = {
    registerReifier(reifier)
    base.deref.doReify(reifier).map(mapping)
  }
}
