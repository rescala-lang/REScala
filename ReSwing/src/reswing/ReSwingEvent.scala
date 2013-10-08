package reswing

import scala.language.implicitConversions
import react.events.Event
import react.events.ImperativeEvent

/**
 * Represents `Swing` events that are fired by the library
 */
final class ReSwingEvent[T] private[reswing] {
  private val event = new ImperativeEvent[T]
  private[reswing] def apply(value: T) = event(value)
}

object ReSwingEvent {
  /**
   * Returns the [[react.events.Event]] representing the event.
   */
  implicit def toEvent[T](value: ReSwingEvent[T]): Event[T] = value.event
}
