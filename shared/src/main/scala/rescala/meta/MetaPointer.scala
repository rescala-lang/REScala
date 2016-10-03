package rescala.meta

import rescala.engines.Ticket
import rescala.graph.{Pulsing, Struct}
import rescala.reactives._

import scala.language.higherKinds

trait MetaPointer[+T] {
  protected val _node : ReactiveNode[T]
  protected var deleted = false
  protected[meta] val node : Option[ReactiveNode[T]] = if (deleted) None else Some(_node)
  def isEmpty = deleted
  def isDefined = node.isDefined

  if (!deleted) _node.graph.addPointer(_node, this)

  def disconnect() : Unit = node match {
    case None => throw new IllegalAccessException("Cannot disconnect a null pointer!")
    case Some(n) => n.graph.addLog(LoggedDisconnect(n))
  }

  protected[meta] def createDependentNode[U]() = {
    node match {
      case None => throw new IllegalAccessException("Cannot create new dependencies for a null pointer!")
      case Some(n) => n.graph.createReactiveNode[U](Set(n))
    }
  }

  protected[meta] def createDependentNode[U](others : Option[ReactiveNode[_]]*) = {
    if (others.exists(_.isEmpty)) throw new IllegalArgumentException("Cannot add a null pointer as a dependency!")
    node match {
      case None => throw new IllegalAccessException("Cannot create new dependencies for a null pointer!")
      case Some(n) => n.graph.createReactiveNode[U](Set(n) ++ others.map(_.get))
    }
  }
}

trait ReactivePointer[+T] extends MetaPointer[T] {
  protected[meta] def reify[S <: Struct](reifier: Reifier[S], skipCollect : Boolean = false): Observable[T, S]

  protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Pulsing[T, S]

  def observe[U >: T](onSuccess: (U) => Unit, onFailure: (Throwable) => Unit = t => throw t): ObservePointer[U] = ObservePointer(createDependentNode(), this, onSuccess, onFailure)
}

trait EventPointer[+T] extends ReactivePointer[T] {
  override def reify[S <: Struct](reifier: Reifier[S], skipCollect : Boolean = false): Event[T, S] = reifier.reifyEvent(this, skipCollect)

  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S]

  def +=[X >: T](react: X => Unit): ObservePointer[X] = ObservePointer(createDependentNode(), this, react)
  def ||[U >: T](others: EventPointer[U]*): OrEventPointer[T, U] = OrEventPointer(createDependentNode(others.map(_.node):_*), this, others:_*)
  def &&[U >: T](pred: (U) => Boolean): FilteredEventPointer[U, U] = FilteredEventPointer[U, U](createDependentNode(), this, pred)
  def \[U](other: EventPointer[U]): ExceptEventPointer[T, U] = ExceptEventPointer(createDependentNode(other.node), this, other)
  def and[X >: T, U, R](other: EventPointer[U])(merger: (X, U) => R): AndEventPointer[X, U, R] = AndEventPointer(createDependentNode(other.node), this, other, merger)
  def zip[U](other: EventPointer[U]): ZippedEventPointer[T, U] = ZippedEventPointer(createDependentNode(other.node), this, other)
  def map[X >: T, U](mapping: (X) => U): MappedEventPointer[X, U] = MappedEventPointer(createDependentNode(), this, mapping)
  def fold[X >: T, A](init: A)(fold: (A, X) => A): FoldedSignalPointer[X, A] = FoldedSignalPointer(createDependentNode(), this, init, fold)
  def toggle[A](a: SignalPointer[A], b: SignalPointer[A]): ToggledSignalPointer[T, A] = ToggledSignalPointer(createDependentNode(a.node, b.node), this, a, b)
  def snapshot[A](s: SignalPointer[A]): SnapshotSignalPointer[T, A] = SnapshotSignalPointer(createDependentNode(s.node), this, s)
  def switchOnce[A](original: SignalPointer[A], newSignal: SignalPointer[A]): SwitchOnceSignalPointer[T, A] = SwitchOnceSignalPointer(createDependentNode(original.node, newSignal.node), this, original, newSignal)
  def switchTo[A >: T](original: SignalPointer[A]): SwitchToSignalPointer[T, A] = SwitchToSignalPointer(createDependentNode(original.node), this, original)
  def flatMap[X >: T, B](f: (X) => EventPointer[B]): FlatMappedEventPointer[X, B] = FlatMappedEventPointer(createDependentNode(), this, f)
}

trait SignalPointer[+A] extends ReactivePointer[A] {
  override def reify[S <: Struct](reifier: Reifier[S], skipCollect : Boolean = false): Signal[A, S] = reifier.reifySignal(this, skipCollect)

  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S]

  def delay(n: Int): DelayedSignalPointer[A] = DelayedSignalPointer(createDependentNode(), this, n)
  def map[X >: A, B](f: (X) => B): MappedSignalPointer[X, B] = MappedSignalPointer(createDependentNode(), this, f)
  def change: ChangeEventPointer[A] = ChangeEventPointer(createDependentNode(), this)
  def changed: ChangedEventPointer[A] = ChangedEventPointer(createDependentNode(), this)
}

case class ObservePointer[T](protected[meta] override val _node : ReactiveNode[Unit], base : ReactivePointer[T], onSuccess: (T) => Unit, onFailure: (Throwable) => Unit = (cause) => { throw cause}) extends MetaPointer[Unit] {
  def reify[S <: Struct](reifier: Reifier[S], skipCollect : Boolean = false): Observe[S] = reifier.reifyObserve(this, skipCollect)

  protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Observe[S] = base.reify(reifier, skipCollect = true).observe(onSuccess, onFailure)
}


case class EvtEventPointer[T](protected[meta] override val _node : ReactiveNode[T]) extends EventPointer[T] {
  override def reify[S <: Struct](reifier: Reifier[S], skipCollect : Boolean = false): Evt[T, S] = reifier.reifyEvt(this, skipCollect)

  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Evt[T, S] = reifier.createEvt(this)

  def fire(value : T) : Unit = node match {
    case Some(n) => n.graph.addLog(LoggedFire(n, value))
    case None => throw new IllegalArgumentException("Cannot fire null pointer!")
  }
}
case class ChangeEventPointer[+T](protected[meta] override val _node : ReactiveNode[Signals.Diff[T]], base : SignalPointer[T]) extends EventPointer[Signals.Diff[T]] {
  override def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[Signals.Diff[T], S] = base.reify(reifier, skipCollect = true).change
}
case class ChangedEventPointer[+T](protected[meta] override val _node : ReactiveNode[T], base : SignalPointer[T]) extends EventPointer[T] {
  override protected [meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.reify(reifier, skipCollect = true).changed
}
case class FilteredEventPointer[T, +U >: T](protected[meta] override val _node : ReactiveNode[T], base : EventPointer[T], pred: (T) => Boolean) extends EventPointer[U] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.reify(reifier, skipCollect = true).filter(pred)
}
case class OrEventPointer[+T <: U, +U](protected[meta] override val _node : ReactiveNode[U], base : EventPointer[T], others : EventPointer[U]*) extends EventPointer[U] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[U, S] = others.foldLeft(base.reify(reifier, skipCollect = true) : Event[U, S])((acc, next) => acc || next.reify(reifier, skipCollect = true))
}
case class ExceptEventPointer[+T, +U](protected[meta] override val _node : ReactiveNode[T], base : EventPointer[T], other : EventPointer[U]) extends EventPointer[T] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.reify(reifier, skipCollect = true) \ other.reify(reifier, skipCollect = true)
}
case class AndEventPointer[T, U, +R](protected[meta] override val _node : ReactiveNode[R], base : EventPointer[T], other : EventPointer[U], merger: (T, U) => R) extends EventPointer[R] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[R, S] = base.reify(reifier, skipCollect = true).and(other.reify(reifier, skipCollect = true))(merger)
}
case class ZippedEventPointer[+T, +U](protected[meta] override val _node : ReactiveNode[(T, U)], base : EventPointer[T], other : EventPointer[U]) extends EventPointer[(T, U)] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[(T, U), S] = base.reify(reifier, skipCollect = true).zip(other.reify(reifier, skipCollect = true))
}
case class MappedEventPointer[T, +U](protected[meta] override val _node : ReactiveNode[U], base : EventPointer[T], mapping: (T) => U) extends EventPointer[U] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[U, S] = base.reify(reifier, skipCollect = true).map(mapping)
}
case class FlatMappedEventPointer[T, +B](protected[meta] override val _node : ReactiveNode[B], base : EventPointer[T], f: (T) => EventPointer[B]) extends EventPointer[B] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[B, S] = base.reify(reifier, skipCollect = true).flatMap(f(_).reify(reifier))
}

case class VarSignalPointer[A](protected[meta] override val _node : ReactiveNode[A]) extends SignalPointer[A] {
  override def reify[S <: Struct](reifier: Reifier[S], skipCollect : Boolean = false): Var[A, S] = reifier.reifyVar(this, skipCollect)

  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Var[A, S] = reifier.createVar(this)

  def set(value : A) : Unit = node match {
    case Some(n) => n.graph.addLog(LoggedSet(n, value))
    case None => throw new IllegalArgumentException("Cannot fire null pointer!")
  }
}
case class FoldedSignalPointer[T, A](protected[meta] override val _node : ReactiveNode[A], base : EventPointer[T], init: A, fold: (A, T) => A) extends SignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier, skipCollect = true).fold(init)(fold)
}
case class ToggledSignalPointer[+T, +A](protected[meta] override val _node : ReactiveNode[A], base : EventPointer[T], a : SignalPointer[A], b : SignalPointer[A]) extends SignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier, skipCollect = true).toggle(a.reify(reifier, skipCollect = true), b.reify(reifier, skipCollect = true))
}
case class SnapshotSignalPointer[+T, +A](protected[meta] override val _node : ReactiveNode[A], base : EventPointer[T], s : SignalPointer[A]) extends SignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier, skipCollect = true).snapshot(s.reify(reifier, skipCollect = true))
}
case class SwitchOnceSignalPointer[+T, +A](protected[meta] override val _node : ReactiveNode[A], base : EventPointer[T], original : SignalPointer[A], newSignal : SignalPointer[A]) extends SignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier, skipCollect = true).switchOnce(original.reify(reifier, skipCollect = true), newSignal.reify(reifier, skipCollect = true))
}
case class SwitchToSignalPointer[+T <: A, +A](protected[meta] override val _node : ReactiveNode[A], base : EventPointer[T], original : SignalPointer[A]) extends SignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier, skipCollect = true).switchTo(original.reify(reifier, skipCollect = true))
}
case class DelayedSignalPointer[+A](protected[meta] override val _node : ReactiveNode[A], base : SignalPointer[A], n: Int) extends SignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier, skipCollect = true).delay(n)
}
case class MappedSignalPointer[A, +U](protected[meta] override val _node : ReactiveNode[U], base : SignalPointer[A], mapping: (A) => U) extends SignalPointer[U] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[U, S] = base.reify(reifier, skipCollect = true).map(mapping)
}
