package rescala.signals

import rescala.events.{Event, Events}
import rescala.propagation.Stateful
import rescala.propagation.turns.Turn
import rescala.propagation.turns.creation.MaybeTurn


trait Signal[+A] extends Stateful[A] {

  // only used inside macro and will be replaced there
  final def apply(): A = throw new IllegalAccessException(s"$this.apply called outside of macro")

  def apply[T](turn: Turn): A = {
    turn.useDependency(this)
    getValue(turn)
  }

  /** Return a Signal with f applied to the value */
  def map[B](f: A => B)(implicit maybe: MaybeTurn): Signal[B] = Signals.mapping(this) { turn => f(getValue(turn)) }

  /** flatten the inner signal */
  def flatten[B]()(implicit ev: A <:< Signal[B], maybe: MaybeTurn) = Signals.dynamic(this) { s => this(s)(s) }

  /** Return a Signal that gets updated only when e fires, and has the value of this Signal */
  def snapshot(e: Event[_])(implicit maybe: MaybeTurn): Signal[A] = e.snapshot(this)

  /** Switch to (and keep) event value on occurrence of e*/
  def switchTo[U >: A](e: Event[U])(implicit maybe: MaybeTurn): Signal[U] = e.switchTo(this)

  /** Switch to (and keep) event value on occurrence of e*/
  def switchOnce[V >: A](e: Event[_])(newSignal: Signal[V])(implicit maybe: MaybeTurn): Signal[V] = e.switchOnce(this, newSignal)

  /** Switch back and forth between this and the other Signal on occurrence of event e */
  def toggle[V >: A](e: Event[_])(other: Signal[V])(implicit maybe: MaybeTurn): Signal[V] = e.toggle(this, other)

  /** Delays this signal by n occurrences */
  def delay(n: Int)(implicit maybe: MaybeTurn): Signal[A] = changed.delay(get, n)

  /** Unwraps a Signal[Event[E]] to an Event[E] */
  def unwrap[E](implicit evidence: A <:< Event[E], maybe: MaybeTurn): Event[E] =  Events.wrapped(this.map(evidence))

  /**
   * Create an event that fires every time the signal changes. It fires the tuple
   *  (oldVal, newVal) for the signal. The first tuple is (null, newVal)
   */
  lazy val change: Event[(A, A)] = Events.change(this)

  /**
   * Create an event that fires every time the signal changes. The value associated
   * to the event is the new value of the signal
   */
  lazy val changed: Event[A] = change.map(_._2)

  /** Convenience function filtering to events which change this reactive to value */
  def changedTo[V](value: V)(implicit maybe: MaybeTurn): Event[Unit] = (changed && { _ == value }).dropParam

}
