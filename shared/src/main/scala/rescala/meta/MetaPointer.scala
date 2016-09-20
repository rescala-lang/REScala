package rescala.meta

import rescala.engines.Ticket
import rescala.graph.Struct
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

trait MetaReactivePointer[+T] extends MetaPointer[T] {
  def observe[U >: T](onSuccess: (U) => Unit, onFailure: (Throwable) => Unit = t => throw new UnhandledFailureException(t)): MetaObservePointer[U] = MetaObservePointer(createDependentNode(), this, onSuccess, onFailure)
}

trait MetaEventPointer[+T] extends MetaReactivePointer[T] {
  def reify[S <: Struct](reifier: Reifier[S]): Event[T, S] = reifier.reifyEvent(this)

  protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S]

  def ||[U >: T](others: MetaEventPointer[U]*): OrEventPointer[T, U] = OrEventPointer(createDependentNode(others.map(_.node):_*), this, others:_*)
  def &&[U >: T](pred: (U) => Boolean): FilteredEventPointer[U, U] = FilteredEventPointer[U, U](createDependentNode(), this, pred)
  def \[U](other: MetaEventPointer[U]): ExceptEventPointer[T, U] = ExceptEventPointer(createDependentNode(other.node), this, other)
  def and[X >: T, U, R](other: MetaEventPointer[U])(merger: (X, U) => R): AndEventPointer[X, U, R] = AndEventPointer(createDependentNode(other.node), this, other, merger)
  def zip[U](other: MetaEventPointer[U]): ZippedEventPointer[T, U] = ZippedEventPointer(createDependentNode(other.node), this, other)
  def map[X >: T, U](mapping: (X) => U): MappedEventPointer[X, U] = MappedEventPointer(createDependentNode(), this, mapping)
  def fold[X >: T, A](init: A)(fold: (A, X) => A): FoldedSignalPointer[X, A] = FoldedSignalPointer(createDependentNode(), this, init, fold)
  def toggle[A](a: MetaSignalPointer[A], b: MetaSignalPointer[A]): ToggledSignalPointer[T, A] = ToggledSignalPointer(createDependentNode(a.node, b.node), this, a, b)
  def snapshot[A](s: MetaSignalPointer[A]): SnapshotSignalPointer[T, A] = SnapshotSignalPointer(createDependentNode(s.node), this, s)
  def switchOnce[A](original: MetaSignalPointer[A], newSignal: MetaSignalPointer[A]): SwitchOnceSignalPointer[T, A] = SwitchOnceSignalPointer(createDependentNode(original.node, newSignal.node), this, original, newSignal)
  def switchTo[A >: T](original: MetaSignalPointer[A]): SwitchToSignalPointer[T, A] = SwitchToSignalPointer(createDependentNode(original.node), this, original)
  def flatMap[X >: T, B](f: (X) => MetaEventPointer[B]): FlatMappedEventPointer[X, B] = FlatMappedEventPointer(createDependentNode(), this, f)
}

trait MetaSignalPointer[+A] extends MetaReactivePointer[A] {
  def reify[S <: Struct](reifier: Reifier[S]): Signal[A, S] = reifier.reifySignal(this)

  protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S]

  def delay(n: Int): DelayedSignalPointer[A] = DelayedSignalPointer(createDependentNode(), this, n)
  def map[X >: A, B](f: (X) => B): MappedSignalPointer[X, B] = MappedSignalPointer(createDependentNode(), this, f)
  def change: ChangeEventPointer[A] = ChangeEventPointer(createDependentNode(), this)
  def changed: ChangedEventPointer[A] = ChangedEventPointer(createDependentNode(), this)
}

case class MetaObservePointer[T](protected[meta] override var node : Option[ReactiveNode], base : MetaReactivePointer[T], onSuccess: (T) => Unit, onFailure: (Throwable) => Unit) extends MetaPointer[Unit] {
}


case class EvtEventPointer[T](protected[meta] override var node : Option[ReactiveNode]) extends MetaEventPointer[T] {
  override def reify[S <: Struct](reifier: Reifier[S]): Evt[T, S] = reifier.reifyEvt(this)

  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Evt[T, S] = reifier.createEvt(this)

  def fire(value : T) : Unit = node match {
    case Some(n) => n.graph.addLog(LoggedFire(n, value))
    case None => throw new IllegalArgumentException("Cannot fire null pointer!")
  }
}
case class ChangeEventPointer[+T](protected[meta] override var node : Option[ReactiveNode], base : MetaSignalPointer[T]) extends MetaEventPointer[Signals.Diff[T]] {
  override def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[Signals.Diff[T], S] = base.reify(reifier).change
}
case class ChangedEventPointer[+T](protected[meta] override var node : Option[ReactiveNode], base : MetaSignalPointer[T]) extends MetaEventPointer[T] {
  override protected [meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.reify(reifier).changed
}
case class FilteredEventPointer[T, +U >: T](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], pred: (T) => Boolean) extends MetaEventPointer[U] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.reify(reifier).filter(pred)
}
case class OrEventPointer[+T <: U, +U](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], others : MetaEventPointer[U]*) extends MetaEventPointer[U] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[U, S] = others.foldLeft(base.reify(reifier) : Event[U, S])((acc, next) => acc || next.reify(reifier))
}
case class ExceptEventPointer[+T, +U](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], other : MetaEventPointer[U]) extends MetaEventPointer[T] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[T, S] = base.reify(reifier) \ other.reify(reifier)
}
case class AndEventPointer[T, U, +R](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], other : MetaEventPointer[U], merger: (T, U) => R) extends MetaEventPointer[R] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[R, S] = base.reify(reifier).and(other.reify(reifier))(merger)
}
case class ZippedEventPointer[+T, +U](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], other : MetaEventPointer[U]) extends MetaEventPointer[(T, U)] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[(T, U), S] = base.reify(reifier).zip(other.reify(reifier))
}
case class MappedEventPointer[T, +U](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], mapping: (T) => U) extends MetaEventPointer[U] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[U, S] = base.reify(reifier).map(mapping)
}
case class FlatMappedEventPointer[T, +B](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], f: (T) => MetaEventPointer[B]) extends MetaEventPointer[B] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Event[B, S] = base.reify(reifier).flatMap(f(_).reify(reifier))
}

case class VarSignalPointer[A](protected[meta] override var node : Option[ReactiveNode]) extends MetaSignalPointer[A] {
  override def reify[S <: Struct](reifier: Reifier[S]): Var[A, S] = reifier.reifyVar(this)

  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Var[A, S] = reifier.createVar(this)

  def set(value : A) : Unit = node match {
    case Some(n) => n.graph.addLog(LoggedSet(n, value))
    case None => throw new IllegalArgumentException("Cannot fire null pointer!")
  }
}
case class FoldedSignalPointer[T, A](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], init: A, fold: (A, T) => A) extends MetaSignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier).fold(init)(fold)
}
case class ToggledSignalPointer[+T, +A](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], a : MetaSignalPointer[A], b : MetaSignalPointer[A]) extends MetaSignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier).toggle(a.reify(reifier), b.reify(reifier))
}
case class SnapshotSignalPointer[+T, +A](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], s : MetaSignalPointer[A]) extends MetaSignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier).snapshot(s.reify(reifier))
}
case class SwitchOnceSignalPointer[+T, +A](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], original : MetaSignalPointer[A], newSignal : MetaSignalPointer[A]) extends MetaSignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier).switchOnce(original.reify(reifier), newSignal.reify(reifier))
}
case class SwitchToSignalPointer[+T <: A, +A](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], original : MetaSignalPointer[A]) extends MetaSignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier).switchTo(original.reify(reifier))
}
case class DelayedSignalPointer[+A](protected[meta] override var node : Option[ReactiveNode], base : MetaSignalPointer[A], n: Int) extends MetaSignalPointer[A] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[A, S] = base.reify(reifier).delay(n)
}
case class MappedSignalPointer[A, +U](protected[meta] override var node : Option[ReactiveNode], base : MetaSignalPointer[A],  mapping: (A) => U) extends MetaSignalPointer[U] {
  override protected[meta] def createReification[S <: Struct](reifier: Reifier[S])(implicit ticket: Ticket[S]): Signal[U, S] = base.reify(reifier).map(mapping)
}
