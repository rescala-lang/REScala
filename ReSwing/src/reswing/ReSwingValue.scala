package reswing

import scala.language.implicitConversions
import react.Signal
import react.events.Event
import react.events.ImperativeEvent

/**
 * Combines reactive values from the application and from the `Swing` library
 */
sealed abstract class ReSwingValue[T] {
  protected val event = new ImperativeEvent[T]
  protected def signal: Signal[T]
  private[reswing] def fixed: Boolean
  private[reswing] def update(setter: (T, T => Unit))
  private[reswing] def update(value: T) = event(value)
}

final case class ReSwingNoValue[T](
    private[reswing] val fixed: Boolean) extends ReSwingValue[T] {
  protected val signal = event latest null.asInstanceOf[T]
  private[reswing] def update(setter: (T, T => Unit)) =
    setter match { case (current, setter) => event(current) }
}

final case class ReSwingValueValue[T](
    private val value: T,
    private[reswing] val fixed: Boolean) extends ReSwingValue[T] {
  protected val signal = event latest value
  private[reswing] def update(setter: (T, T => Unit)) =
    setter match { case (current, setter) => setter(value); event(value) }
}

final case class ReSwingEventValue[T](
    private val value: Event[T],
    private[reswing] val fixed: Boolean) extends ReSwingValue[T] {
  protected val signal = (value || event) latest null.asInstanceOf[T]
  private[reswing] def update(setter: (T, T => Unit)) =
    setter match { case (current, setter) => value += setter; event(current) }
}

final case class ReSwingSignalValue[T](
    private val value: Signal[T],
    private[reswing] val fixed: Boolean) extends ReSwingValue[T] {
  protected val signal = (value.changed || event) latest value.getValue
  private[reswing] def update(setter: (T, T => Unit)) =
    setter match { case (current, setter) =>
      value.changed += setter; setter(value.getValue) }
}

object ReSwingValue {
  /**
   * Does not cause the `Swing` library to use a specific value.
   */
  implicit def toReSwingValue[T](value: Unit) = ReSwingNoValue[T](false)
  
  /**
   * Sets the given value once.
   * After this, does not cause the `Swing` library to use a specific value.
   */
  implicit def toReSwingValue[T](value: T) = ReSwingValueValue(value, false)
  
  /**
   * Sets the value whenever the given [[react.events.Event]] changes.
   */
  implicit def toReSwingValue[T](value: Event[T]) = ReSwingEventValue(value, false)
  
  /**
   * Sets the value to the value of the given [[react.Signal]] and causes
   * the `Swing` library to always use the current `Signal` value.
   */
  implicit def toReSwingValue[T](value: Signal[T]) = ReSwingSignalValue(value, true)
  
  /**
   * Returns the [[react.Signal]] representing the value.
   */
  implicit def toSignal[T](value: ReSwingValue[T]) = value.signal
}
