package reswing

import scala.language.implicitConversions
import react.Signal
import react.events.Event
import react.events.ImperativeEvent

sealed abstract class ReSwingValue[T] {
  protected val event = new ImperativeEvent[T]
  protected def signal: Signal[T]
  private[reswing] def fixed: Boolean
  private[reswing] def apply(current: T, setter: T => Unit)
  private[reswing] def update(value: T) = event(value)
}

final case class ReSwingNoValue[T](
    private[reswing] val fixed: Boolean) extends ReSwingValue[T] {
  protected val signal = event latest null.asInstanceOf[T]
  private[reswing] def apply(current: T, setter: T => Unit) {
    event(current)
  }
}

final case class ReSwingValueValue[T](
    private val value: T,
    private[reswing] val fixed: Boolean) extends ReSwingValue[T] {
  protected val signal = event latest value
  private[reswing] def apply(current: T, setter: T => Unit) {
    setter(value)
    event(value)
  }
}

final case class ReSwingEventValue[T](
    private val value: Event[T],
    private[reswing] val fixed: Boolean) extends ReSwingValue[T] {
  protected val signal = (value || event) latest null.asInstanceOf[T]
  private[reswing] def apply(current: T, setter: T => Unit) {
    value += setter
    event(current)
  }
}

final case class ReSwingSignalValue[T](
    private val value: Signal[T],
    private[reswing] val fixed: Boolean) extends ReSwingValue[T] {
  protected val signal = (value.changed || event) latest value.getValue
  private[reswing] def apply(current: T, setter: T => Unit) {
    value.changed += setter
    setter(value.getValue)
  }
}

object ReSwingValue {
  implicit def toReSwingValue[T](value: Unit) = ReSwingNoValue[T](false)
  implicit def toReSwingValue[T](value: T) = ReSwingValueValue(value, false)
  implicit def toReSwingValue[T](value: Event[T]) = ReSwingEventValue(value, false)
  implicit def toReSwingValue[T](value: Signal[T]) = ReSwingSignalValue(value, true)
  implicit def toSignal[T](value: ReSwingValue[T]) = value.signal
}
