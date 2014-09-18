package rescala.signals

import rescala.propagation.Pulse.{Diff, NoChange}
import rescala.propagation._
import rescala._
import rescala.events.{Event, Events}


trait Signal[+A] extends Dependency[A] {

  protected[this] var currentValue: A = _

  override def pulse(implicit turn: Turn): Pulse[A] = super.pulse match {
    case Pulse.none => NoChange(Some(get(turn)))
    case other => other
  }

  override def commit(implicit turn: Turn): Unit = {
    pulse.toOption.foreach(currentValue = _)
    super.commit
  }

  def get(implicit turn: MaybeTurn): A = turn.turn match {
    case Some(x) => get(x)
    case None => currentValue
  }

  def get(turn: Turn): A = pulse(turn).toOption.get

  // only used inside macro and will be replaced there
  final def apply(): A = throw new IllegalAccessException(s"$this.apply called outside of macro")

  def apply[T](turn: Turn): A = {
    turn.dynamic.used(this)
    pulse(turn).toOption.get
  }

  def map[B](f: A => B): Signal[B] = StaticSignal.turn(this) { turn => f(get(turn)) }

  /** Return a Signal that gets updated only when e fires, and has the value of this Signal */
  def snapshot(e: Event[_]): Signal[A] = e.snapshot(this)

  /** Switch to (and keep) event value on occurrence of e*/
  def switchTo[U >: A](e: Event[U]): Signal[U] = e.switchTo(this)

  /** Switch to (and keep) event value on occurrence of e*/
  def switchOnce[V >: A](e: Event[_])(newSignal: Signal[V]): Signal[V] = e.switchOnce(this, newSignal)

  /** Switch back and forth between this and the other Signal on occurrence of event e */
  def toggle[V >: A](e: Event[_])(other: Signal[V]): Signal[V] = e.toggle(this, other)

  /** Delays this signal by n occurrences */
  def delay(n: Int): Signal[A] = changed.delay(get, n)

  /** Unwraps a Signal[Event[E]] to an Event[E] */
  def unwrap[E](implicit evidence: A <:< Event[E]): Event[E] =  Events.wrapped(this.map(evidence))

  /**
   * Create an event that fires every time the signal changes. It fires the tuple
   *  (oldVal, newVal) for the signal. The first tuple is (null, newVal)
   */
  lazy val change: Event[(A, A)] = Events.changed(this)

  /**
   * Create an event that fires every time the signal changes. The value associated
   * to the event is the new value of the signal
   */
  lazy val changed: Event[A] = change.map[A, (A, A)](_._2)

  /** Convenience function filtering to events which change this reactive to value */
  def changedTo[V](value: V): Event[Unit] = (changed && { _ == value }).dropParam

}
