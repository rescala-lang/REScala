package reswing

import scala.language.implicitConversions
import react.events.Event
import react.events.ImperativeEvent

final class ReSwingEvent[T] private[reswing] {
  private val event = new ImperativeEvent[T]
  private[reswing] def apply(value: T) = event(value)
}

object ReSwingEvent {
  implicit def toEvent[T](value: ReSwingEvent[T]): Event[T] = value.event
}
