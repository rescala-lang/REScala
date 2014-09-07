package rescala.signals

import rescala.{IFunctions, _}
import rescala.events.Event

trait Signal[+A] extends Changing[A] with FoldableReactive[A] with DepHolder {
  override def fold[B](init: B)(f: (B, A) => B): Signal[B] =
    new FoldedSignal(changed, init, f)

  def get: A

  final def apply(): A = get

  /** hook for subclasses to do something when they use their dependencies */
  def onDynamicDependencyUse[T](dependency: Signal[T]): Unit = { }

  def apply[T](signal: SignalSynt[T]): A = {
    signal.onDynamicDependencyUse(this)
    get
  }

  def map[B](f: A => B): Signal[B] = SignalSynt(this) { s: SignalSynt[B] => f(apply(s)) }

  /** Return a Signal that gets updated only when e fires, and has the value of this Signal */
  def snapshot(e: Event[_]): Signal[A] = IFunctions.snapshot(e, this)

  /** Switch to (and keep) event value on occurrence of e*/ // TODO: check types
  def switchTo[U >: A](e: Event[U]): Signal[U] = IFunctions.switchTo(e, this)

  /** Switch to (and keep) event value on occurrence of e*/ // TODO: check types
  def switchOnce[V >: A](e: Event[_])(newSignal: Signal[V]): Signal[V] = IFunctions.switchOnce(e, this, newSignal)

  /** Switch back and forth between this and the other Signal on occurrence of event e */
  def toggle[V >: A](e: Event[_])(other: Signal[V]): Signal[V] = IFunctions.toggle(e, this, other)

  /** Delays this signal by n occurrences */
  def delay(n: Int): Signal[A] = IFunctions.delay(this, n)

  /** Unwraps a Signal[Event[E]] to an Event[E] */
  def unwrap[E](implicit evidence: A <:< Event[E]): Event[E] = IFunctions.unwrap(this.map(evidence))

}
