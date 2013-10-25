package reswing

import scala.language.implicitConversions
import react.events.Event
import react.events.ImperativeEvent

/**
 * Represents `Swing` events that are fired by the library
 */
final class ReSwingEvent[T] private[reswing] (init: ReSwingEvent[T] => Unit) {
  private val event = Lazy { new ImperativeEvent[T] }
  private def toEvent = { if (!event.isDefined) init(this); event() }
  private[reswing] def apply(value: T) = if (event.isDefined) event()(value)
}

object ReSwingEvent {
  /**
   * Returns the [[react.events.Event]] representing the event.
   */
  implicit def toEvent[T](value: ReSwingEvent[T]): Event[T] = value.toEvent
}
