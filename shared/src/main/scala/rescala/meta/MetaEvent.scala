package rescala.meta

import rescala.graph.Struct
import rescala.reactives.RExceptions.UnhandledFailureException
import rescala.reactives.{EventLike, SignalLike}

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
  def reify[S <: Struct, SL[+X, Z <: Struct] <: SignalLike[X, Z, SL, EV], EV[+X, Z <: Struct] <: EventLike[X, Z, SL, EV]](reifier: Reifier[S, SL, EV]): EV[T, S] = reifier.reifyEvent(this)

  def ||[U >: T](other: MetaEventPointer[U]): OrEventPointer[T, U] = OrEventPointer(createDependentNode(other.node), this, other)
  def &&[U >: T](pred: (U) => Boolean): FilteredEventPointer[T, U] = FilteredEventPointer(createDependentNode(), this, pred)
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
  def reify[S <: Struct, SL[+X, Z <: Struct] <: SignalLike[X, Z, SL, EV], EV[+X, Z <: Struct] <: EventLike[X, Z, SL, EV]](reifier: Reifier[S, SL, EV]): SL[A, S] = reifier.reifySignal(this)

  def delay(n: Int): DelayedSignalPointer[A] = DelayedSignalPointer(createDependentNode(), this, n)
  def map[X >: A, B](f: (X) => B): MappedSignalPointer[X, B] = MappedSignalPointer(createDependentNode(), this, f)
  def flatten[B]()(implicit evidence: <:<[A, MetaSignalPointer[B]]): FlattenedSignalPointer[A, B] = FlattenedSignalPointer(createDependentNode(), this)
  def unwrap[E](implicit evidence: <:<[A, MetaEventPointer[E]]): FlattenedEventPointer[A, E] = FlattenedEventPointer(createDependentNode(), this)
  def change: ChangeEventPointer[A] = ChangeEventPointer(createDependentNode(), this)
  def changed: ChangedEventPointer[A] = ChangedEventPointer(createDependentNode(), this)
}

case class MetaObservePointer[T](protected[meta] override var node : Option[ReactiveNode], base : MetaReactivePointer[T], onSuccess: (T) => Unit, onFailure: (Throwable) => Unit) extends MetaPointer[Unit] {
}


case class EvtEventPointer[T](protected[meta] override var node : Option[ReactiveNode]) extends MetaEventPointer[T] {
  def fire(value : T) : Unit = node match {
    case Some(n) => n.graph.addLog(LoggedFire(n, value))
    case None => throw new IllegalArgumentException("Cannot fire null pointer!")
  }
}
case class ChangeEventPointer[+T](protected[meta] override var node : Option[ReactiveNode], base : MetaSignalPointer[T]) extends MetaEventPointer[(T, T)]
case class ChangedEventPointer[+T](protected[meta] override var node : Option[ReactiveNode], base : MetaSignalPointer[T]) extends MetaEventPointer[T]
case class FilteredEventPointer[+T, U](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], pred: (U) => Boolean) extends MetaEventPointer[U]
case class OrEventPointer[+T, +U](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], other : MetaEventPointer[U]) extends MetaEventPointer[U]
case class ExceptEventPointer[+T, +U](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], other : MetaEventPointer[U]) extends MetaEventPointer[T]
case class AndEventPointer[T, U, +R](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], other : MetaEventPointer[U], merger: (T, U) => R) extends MetaEventPointer[R]
case class ZippedEventPointer[+T, +U](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], other : MetaEventPointer[U]) extends MetaEventPointer[(T, U)]
case class MappedEventPointer[T, +U](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], mapping: (T) => U) extends MetaEventPointer[U]
case class FlatMappedEventPointer[T, +B](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], f: (T) => MetaEventPointer[B]) extends MetaEventPointer[B]
case class FlattenedEventPointer[+A, +T](protected[meta] override var node : Option[ReactiveNode], base : MetaSignalPointer[A]) extends MetaEventPointer[T]

case class VarSignalPointer[A](protected[meta] override var node : Option[ReactiveNode]) extends MetaSignalPointer[A] {
  def set(value : A) : Unit = node match {
    case Some(n) => n.graph.addLog(LoggedSet(n, value))
    case None => throw new IllegalArgumentException("Cannot fire null pointer!")
  }
}
case class FoldedSignalPointer[T, A](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], init: A, fold: (A, T) => A) extends MetaSignalPointer[A]
case class ToggledSignalPointer[+T, +A](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], a : MetaSignalPointer[A], b : MetaSignalPointer[A]) extends MetaSignalPointer[A]
case class SnapshotSignalPointer[+T, +A](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], s : MetaSignalPointer[A]) extends MetaSignalPointer[A]
case class SwitchOnceSignalPointer[+T, +A](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], original : MetaSignalPointer[A], newSignal : MetaSignalPointer[A]) extends MetaSignalPointer[A]
case class SwitchToSignalPointer[+T, +A](protected[meta] override var node : Option[ReactiveNode], base : MetaEventPointer[T], original : MetaSignalPointer[A]) extends MetaSignalPointer[A]
case class DelayedSignalPointer[+A](protected[meta] override var node : Option[ReactiveNode], base : MetaSignalPointer[A], n: Int) extends MetaSignalPointer[A]
case class MappedSignalPointer[A, +U](protected[meta] override var node : Option[ReactiveNode], base : MetaSignalPointer[A],  mapping: (A) => U) extends MetaSignalPointer[U]
case class FlattenedSignalPointer[+A, +B](protected[meta] override var node : Option[ReactiveNode], base : MetaSignalPointer[A]) extends MetaSignalPointer[B]
