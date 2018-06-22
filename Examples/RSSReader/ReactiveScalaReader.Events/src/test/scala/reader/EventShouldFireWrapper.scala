package reader

import rescala.default._

import scala.language.implicitConversions

class EventShouldFireWrapper[T](evt: Event[T]) {
  def shouldFireIn(action: => Unit) = {
    var fired = false
    evt += { _ => fired = true }

    action

    assert(fired, "Event should have fired, but did not")
  }

  def shouldNotFireIn(action: => Unit) = {
    var fired = false
    evt += { _ => fired = true }

    action

    assert(!fired, "Event should not have fired, but it did")
  }
}

object EventShouldFireWrapper {
  implicit def convertToEventShouldFireWrapper[T](evt: Event[T]) =
    new EventShouldFireWrapper(evt)
}
