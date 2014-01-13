package reswing

import scala.language.implicitConversions
import react.events.Event
import react.events.ImperativeEvent

/**
 * Represents `Swing` events that are fired by the library or passed to the
 * library.
 */
sealed abstract class ReSwingEvent[T] {
  private[reswing] def toEvent: Event[T]
}

final class ReSwingEventOut[T] private[reswing]
    (initLazily: ReSwingEventOut[T] => Unit) extends ReSwingEvent[T] {
  private val event = Lazy { new ImperativeEvent[T] }
  private[reswing] def toEvent = { if (!event.isDefined) initLazily(this); event() }
  private[reswing] def apply(value: T) = if (event.isDefined) event()(value)
}

final class ReSwingEventIn[T] private[reswing]
    (event: Lazy[Event[T]]) extends ReSwingEvent[T] {
  private[reswing] def toEvent = { event() }
}

final class ReSwingEventNone[T] private[reswing] extends ReSwingEvent[T] {
  private[reswing] def toEvent = null
}

object ReSwingEvent {
  /**
   * Creates an empty event (that is never fired) to be used with the library.
   */
  implicit def apply[T](value: Unit) = new ReSwingEventNone[T]
  
  /**
   * Wraps a [[react.events.Event]] to be used with the library.
   */
  implicit def apply[T](value: => Event[T]) = new ReSwingEventIn(Lazy { value })
  
  /**
   * Returns the [[react.events.Event]] representing the event.
   */
  implicit def toEvent[T](value: ReSwingEvent[T]): Event[T] = value.toEvent
}
