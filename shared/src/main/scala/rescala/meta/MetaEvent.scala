package rescala.meta

import scala.util.Try

trait MetaPointer[T] {
  val node : ReactiveNode

  protected def createDependentNode() = {
    node.graph.createReactiveNode(Set(node))
  }

  protected def createDependentNode(others : ReactiveNode*) = {
    node.graph.createReactiveNode(Set(node) ++ others)
  }
}

trait MetaReactivePointer[T] extends MetaPointer[T] {
  def observe(onSuccess: (T) => Unit, onFailure: (Throwable) => Unit): MetaObservePointer[T] = MetaObservePointer(createDependentNode(), this, onSuccess, onFailure)
}

trait MetaEventPointer[T] extends MetaPointer[T] {
  def toTry: TryEventPointer[T] = TryEventPointer(createDependentNode(), this)
  def ||[U >: T](other: MetaEventPointer[U]): OrEventPointer[T, U] = OrEventPointer(createDependentNode(other.node), this, other)
  def &&(pred: (T) => Boolean): FilteredEventPointer[T] = FilteredEventPointer(createDependentNode(), this, pred)
  def \[U](other: MetaEventPointer[U]): ExceptEventPointer[T, U] = ExceptEventPointer(createDependentNode(other.node), this, other)
  def and[U, R](other: MetaEventPointer[U])(merger: (T, U) => R): AndEventPointer[T, U, R] = AndEventPointer(createDependentNode(other.node), this, other, merger)
  def zip[U](other: MetaEventPointer[U]): ZippedEventPointer[T, U] = ZippedEventPointer(createDependentNode(other.node), this, other)
  def map[U](mapping: (T) => U): MappedEventPointer[T, U] = MappedEventPointer(createDependentNode(), this, mapping)
  def fold[A](init: A)(fold: (A, T) => A): FoldedSignalPointer[T, A] = FoldedSignalPointer(createDependentNode(), this, init, fold)
  def toggle[A](a: MetaSignalPointer[A], b: MetaSignalPointer[A]): ToggledSignalPointer[T, A] = ToggledSignalPointer(createDependentNode(a.node, b.node), this, a, b)
  def snapshot[A](s: MetaSignalPointer[A]): SnapshotSignalPointer[T, A] = SnapshotSignalPointer(createDependentNode(s.node), this, s)
  def switchOnce[A](original: MetaSignalPointer[A], newSignal: MetaSignalPointer[A]): SwitchOnceSignalPointer[T, A] = SwitchOnceSignalPointer(createDependentNode(original.node, newSignal.node), this, original, newSignal)
  def switchTo[A >: T](original: MetaSignalPointer[A]): SwitchToSignalPointer[T, A] = SwitchToSignalPointer(createDependentNode(original.node), this, original)
  def flatMap[B](f: (T) => MetaEventPointer[B]): FlatMappedEventPointer[T, B] = FlatMappedEventPointer(createDependentNode(), this, f)
}

trait MetaSignalPointer[A] extends MetaPointer[A] {
  def delay(n: Int): DelayedSignalPointer[A] = DelayedSignalPointer(createDependentNode(), this, n)
  def toTry: TrySignalPointer[A] = TrySignalPointer(createDependentNode(), this)
  def map[B](f: (A) => B): MappedSignalPointer[A, B] = MappedSignalPointer(createDependentNode(), this, f)
  def flatten[B]()(implicit evidence: <:<[A, MetaSignalPointer[B]]): FlattenedSignalPointer[A, B] = FlattenedSignalPointer(createDependentNode(), this)
  def unwrap[E](implicit evidence: <:<[A, MetaEventPointer[E]]): UnwrappedEventPointer[A, E] = UnwrappedEventPointer(createDependentNode(), this)
  def change: ChangeEventPointer[A] = ChangeEventPointer(createDependentNode(), this)
}

case class MetaObservePointer[T](override val node : ReactiveNode, base : MetaReactivePointer[T], onSuccess: (T) => Unit, onFailure: (Throwable) => Unit) extends MetaPointer[Unit] {
}

case class EvtEventPointer[T](override val node : ReactiveNode) extends MetaEventPointer[T]
case class ChangeEventPointer[T](override val node : ReactiveNode, base : MetaSignalPointer[T]) extends MetaEventPointer[(T, T)]
case class FilteredEventPointer[T](override val node : ReactiveNode, base : MetaEventPointer[T], pred: (T) => Boolean) extends MetaEventPointer[T]
case class OrEventPointer[T, U >: T](override val node : ReactiveNode, base : MetaEventPointer[T], other : MetaEventPointer[U]) extends MetaEventPointer[U]
case class ExceptEventPointer[T, U](override val node : ReactiveNode, base : MetaEventPointer[T], other : MetaEventPointer[U]) extends MetaEventPointer[T]
case class AndEventPointer[T, U, R](override val node : ReactiveNode, base : MetaEventPointer[T], other : MetaEventPointer[U], merger: (T, U) => R) extends MetaEventPointer[T]
case class ZippedEventPointer[T, U](override val node : ReactiveNode, base : MetaEventPointer[T], other : MetaEventPointer[U]) extends MetaEventPointer[(T, U)]
case class MappedEventPointer[T, U](override val node : ReactiveNode, base : MetaEventPointer[T], mapping: (T) => U) extends MetaEventPointer[U]
case class FlatMappedEventPointer[T, B](override val node : ReactiveNode, base : MetaEventPointer[T], f: (T) => MetaEventPointer[B]) extends MetaEventPointer[B]
case class UnwrappedEventPointer[A, T](override val node : ReactiveNode, base : MetaSignalPointer[A]) extends MetaEventPointer[T]
case class TryEventPointer[T](override val node : ReactiveNode, base : MetaEventPointer[T]) extends MetaEventPointer[Try[T]]

case class VarSignalPointer[A](override val node : ReactiveNode) extends MetaSignalPointer[A]
case class FoldedSignalPointer[T, A](override val node : ReactiveNode, base : MetaEventPointer[T], init: A, fold: (A, T) => A) extends MetaSignalPointer[A]
case class ToggledSignalPointer[T, A](override val node : ReactiveNode, base : MetaEventPointer[T], a : MetaSignalPointer[A], b : MetaSignalPointer[A]) extends MetaSignalPointer[A]
case class SnapshotSignalPointer[T, A](override val node : ReactiveNode, base : MetaEventPointer[T], s : MetaSignalPointer[A]) extends MetaSignalPointer[A]
case class SwitchOnceSignalPointer[T, A](override val node : ReactiveNode, base : MetaEventPointer[T], original : MetaSignalPointer[A], newSignal : MetaSignalPointer[A]) extends MetaSignalPointer[A]
case class SwitchToSignalPointer[T, A >: T](override val node : ReactiveNode, base : MetaEventPointer[T], original : MetaSignalPointer[A]) extends MetaSignalPointer[A]
case class DelayedSignalPointer[A](override val node : ReactiveNode, base : MetaSignalPointer[A], n: Int) extends MetaSignalPointer[A]
case class MappedSignalPointer[A, U](override val node : ReactiveNode, base : MetaSignalPointer[A],  mapping: (A) => U) extends MetaSignalPointer[U]
case class FlattenedSignalPointer[A, B](override val node : ReactiveNode, base : MetaSignalPointer[A]) extends MetaSignalPointer[B]
case class TrySignalPointer[A](override val node : ReactiveNode, base : MetaSignalPointer[A]) extends MetaSignalPointer[Try[A]]
