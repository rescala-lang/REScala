package rescala.meta

import rescala.engines.Ticket
import rescala.graph.{Pulsing, Struct}
import rescala.reactives.RExceptions.UnhandledFailureException
import rescala.reactives._

import scala.language.higherKinds

trait MetaPointer[+T] {
  protected[meta] var node : Option[ReactiveNode]
  def isEmpty = node.isEmpty
  def isDefined = node.isDefined

  protected[meta] def createDependentNode() = {
    node match {
      case None => throw new IllegalAccessException("Cannot create new dependencies for a null pointer!")
      case Some(n) => n.graph.createReactiveNode(Set(n))
    }
  }

  protected[meta] def createDependentNode(others : ReactiveNode*) = {
    node match {
      case None => throw new IllegalAccessException("Cannot create new dependencies for a null pointer!")
      case Some(n) => n.graph.createReactiveNode(Set(n) ++ others)
    }
  }

  protected[meta] def createDependentNode(others : Option[ReactiveNode]*) = {
    if (others.exists(_.isEmpty)) throw new IllegalArgumentException("Cannot add a null pointer as a dependency!")
    node match {
      case None => throw new IllegalAccessException("Cannot create new dependencies for a null pointer!")
      case Some(n) => Some(n.graph.createReactiveNode(Set(n) ++ others.map(_.get)))
    }
  }

  protected[meta] def merge[U >: T](others : MetaPointer[U]*): Unit = {
    node match {
      case None => throw new IllegalAccessException("Cannot merge into a null pointer!")
      case Some(n) => val mergedNode = n.graph.mergeNodes((others.map(_.node.getOrElse(n)) :+ n).toSet)
        node = Some(mergedNode)
        others.foreach(_.node = None)
    }

  }
}

trait ReactivePointer[+T] extends MetaPointer[T] {
  def reify[S <: Struct](reifier: Reifier[S]): Observable[T, S]

  protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Pulsing[T, S]

  def observe[U >: T](onSuccess: (U) => Unit, onFailure: (Throwable) => Unit = t => throw new UnhandledFailureException(t)): ObservePointer[U] = ObservePointer(createDependentNode(), this, onSuccess, onFailure)
}

trait EventPointer[+T] extends ReactivePointer[T] {
  override def reify[S <: Struct](reifier: Reifier[S]): Event[T, S] = reifier.reifyEvent(this)

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
  override def reify[S <: Struct](reifier: Reifier[S]): Signal[A, S] = reifier.reifySignal(this)

  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S]

  def delay(n: Int): DelayedSignalPointer[A] = DelayedSignalPointer(createDependentNode(), this, n)
  def map[X >: A, B](f: (X) => B): MappedSignalPointer[X, B] = MappedSignalPointer(createDependentNode(), this, f)
  def change: ChangeEventPointer[A] = ChangeEventPointer(createDependentNode(), this)
  def changed: ChangedEventPointer[A] = ChangedEventPointer(createDependentNode(), this)
}

case class ObservePointer[T](protected[meta] override var node : Option[ReactiveNode], base : ReactivePointer[T], onSuccess: (T) => Unit, onFailure: (Throwable) => Unit = (cause) => { throw new UnhandledFailureException(cause)}) extends MetaPointer[Unit] {
  def reify[S <: Struct](reifier: Reifier[S]): Observe[S] = reifier.reifyObserve(this)

  protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Observe[S] = base.reify(reifier).observe(onSuccess, onFailure)
}


case class EvtEventPointer[T](protected[meta] override var node : Option[ReactiveNode]) extends EventPointer[T] {
  override def reify[S <: Struct](reifier: Reifier[S]): Evt[T, S] = reifier.reifyEvt(this)

  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Evt[T, S] = reifier.createEvt(this)

  def fire(value : T) : Unit = node match {
    case Some(n) => n.graph.addLog(LoggedFire(n, value))
    case None => throw new IllegalArgumentException("Cannot fire null pointer!")
  }
}
case class ChangeEventPointer[+T](protected[meta] override var node : Option[ReactiveNode], base : SignalPointer[T]) extends EventPointer[Signals.Diff[T]] {
  override def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[Signals.Diff[T], S] = base.reify(reifier).change
}
case class ChangedEventPointer[+T](protected[meta] override var node : Option[ReactiveNode], base : SignalPointer[T]) extends EventPointer[T] {
  override protected [meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.reify(reifier).changed
}
case class FilteredEventPointer[T, +U >: T](protected[meta] override var node : Option[ReactiveNode], base : EventPointer[T], pred: (T) => Boolean) extends EventPointer[U] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.reify(reifier).filter(pred)
}
case class OrEventPointer[+T <: U, +U](protected[meta] override var node : Option[ReactiveNode], base : EventPointer[T], others : EventPointer[U]*) extends EventPointer[U] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[U, S] = others.foldLeft(base.reify(reifier) : Event[U, S])((acc, next) => acc || next.reify(reifier))
}
case class ExceptEventPointer[+T, +U](protected[meta] override var node : Option[ReactiveNode], base : EventPointer[T], other : EventPointer[U]) extends EventPointer[T] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.reify(reifier) \ other.reify(reifier)
}
case class AndEventPointer[T, U, +R](protected[meta] override var node : Option[ReactiveNode], base : EventPointer[T], other : EventPointer[U], merger: (T, U) => R) extends EventPointer[R] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[R, S] = base.reify(reifier).and(other.reify(reifier))(merger)
}
case class ZippedEventPointer[+T, +U](protected[meta] override var node : Option[ReactiveNode], base : EventPointer[T], other : EventPointer[U]) extends EventPointer[(T, U)] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[(T, U), S] = base.reify(reifier).zip(other.reify(reifier))
}
case class MappedEventPointer[T, +U](protected[meta] override var node : Option[ReactiveNode], base : EventPointer[T], mapping: (T) => U) extends EventPointer[U] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[U, S] = base.reify(reifier).map(mapping)
}
case class FlatMappedEventPointer[T, +B](protected[meta] override var node : Option[ReactiveNode], base : EventPointer[T], f: (T) => EventPointer[B]) extends EventPointer[B] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[B, S] = base.reify(reifier).flatMap(f(_).reify(reifier))
}

case class VarSignalPointer[A](protected[meta] override var node : Option[ReactiveNode]) extends SignalPointer[A] {
  override def reify[S <: Struct](reifier: Reifier[S]): Var[A, S] = reifier.reifyVar(this)

  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Var[A, S] = reifier.createVar(this)

  def set(value : A) : Unit = node match {
    case Some(n) => n.graph.addLog(LoggedSet(n, value))
    case None => throw new IllegalArgumentException("Cannot fire null pointer!")
  }
}
case class FoldedSignalPointer[T, A](protected[meta] override var node : Option[ReactiveNode], base : EventPointer[T], init: A, fold: (A, T) => A) extends SignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier).fold(init)(fold)
}
case class ToggledSignalPointer[+T, +A](protected[meta] override var node : Option[ReactiveNode], base : EventPointer[T], a : SignalPointer[A], b : SignalPointer[A]) extends SignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier).toggle(a.reify(reifier), b.reify(reifier))
}
case class SnapshotSignalPointer[+T, +A](protected[meta] override var node : Option[ReactiveNode], base : EventPointer[T], s : SignalPointer[A]) extends SignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier).snapshot(s.reify(reifier))
}
case class SwitchOnceSignalPointer[+T, +A](protected[meta] override var node : Option[ReactiveNode], base : EventPointer[T], original : SignalPointer[A], newSignal : SignalPointer[A]) extends SignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier).switchOnce(original.reify(reifier), newSignal.reify(reifier))
}
case class SwitchToSignalPointer[+T <: A, +A](protected[meta] override var node : Option[ReactiveNode], base : EventPointer[T], original : SignalPointer[A]) extends SignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier).switchTo(original.reify(reifier))
}
case class DelayedSignalPointer[+A](protected[meta] override var node : Option[ReactiveNode], base : SignalPointer[A], n: Int) extends SignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier).delay(n)
}
case class MappedSignalPointer[A, +U](protected[meta] override var node : Option[ReactiveNode], base : SignalPointer[A], mapping: (A) => U) extends SignalPointer[U] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[U, S] = base.reify(reifier).map(mapping)
}
