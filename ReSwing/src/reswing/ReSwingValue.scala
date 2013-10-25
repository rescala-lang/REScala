package reswing

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import react.Signal
import react.events.Event
import react.events.ImperativeEvent

/**
 * Combines reactive values from the application and from the `Swing` library
 */
sealed abstract class ReSwingValue[T] {
  protected def signal: Lazy[Signal[T]]
  protected val event = Lazy { new ImperativeEvent[T] }
  protected var latestValue = null.asInstanceOf[T]
  
  final private val inits = ListBuffer.empty[Unit => Unit]
  
  final protected def toSignal = {
    if (!signal.isDefined) {
      signal()
      for (init <- inits)
        init()
      inits.clear
    }
    signal()
  }
  
  private[reswing] def fixed: Boolean
  private[reswing] def getValue: T
  private[reswing] def use(setter: T => Unit)
  
  final private[reswing] def update(value: T)
    { latestValue = value; if (event.isDefined) event()(value) }
  final private[reswing] def init(init: Unit => Unit)
    { if (signal.isDefined) init() else inits += init }
}

final case class ReSwingNoValue[T]() extends ReSwingValue[T] {
  protected val signal = Lazy { event() latest latestValue }
  private[reswing] def fixed = false
  private[reswing] def getValue = latestValue
  private[reswing] def use(setter: T => Unit) { }
}

final case class ReSwingValueValue[T](private val value: T) extends ReSwingValue[T] {
  protected val signal = Lazy { event() latest latestValue }
  private[reswing] def fixed = false
  private[reswing] def getValue = latestValue
  private[reswing] def use(setter: T => Unit) = setter(value)
}

final case class ReSwingEventValue[T](private val value: Event[T]) extends ReSwingValue[T] {
  protected val signal = Lazy { (value || event()) latest latestValue }
  private[reswing] def fixed = false
  private[reswing] def getValue = latestValue
  private[reswing] def use(setter: T => Unit) = value += setter
}

final case class ReSwingSignalValue[T](private val value: Signal[T]) extends ReSwingValue[T] {
  protected val signal = Lazy { (value.changed || event()) latest value.getValue }
  private[reswing] def fixed = true
  private[reswing] def getValue = value.getValue
  private[reswing] def use(setter: T => Unit) { value.changed += setter; setter(value.getValue) }
}

object ReSwingValue {
  /**
   * Does not cause the `Swing` library to use a specific value.
   */
  implicit def toReSwingValue[T](value: Unit) = ReSwingNoValue[T]
  
  /**
   * Sets the given value once.
   * After this, does not cause the `Swing` library to use a specific value.
   */
  implicit def toReSwingValue[T](value: T) = ReSwingValueValue(value)
  
  /**
   * Sets the value whenever the given [[react.events.Event]] changes.
   */
  implicit def toReSwingValue[T](value: Event[T]) = ReSwingEventValue(value)
  
  /**
   * Sets the value to the value of the given [[react.Signal]] and causes
   * the `Swing` library to always use the current `Signal` value.
   */
  implicit def toReSwingValue[T](value: Signal[T]) = ReSwingSignalValue(value)
  
  /**
   * Returns the [[react.Signal]] representing the value.
   */
  implicit def toSignal[T](value: ReSwingValue[T]) = value.toSignal
}
