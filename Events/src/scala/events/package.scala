package scala

package object events {

  def between[T,U](start: Event[T], end: Event[U]) = new BetweenEvent(start, end)

  def within[T,U](ie: IntervalEvent[T,U]) = new WithinEvent(ie)

  def causedBy[T](e: Event[T]) = new CausedByFilter(e)

  def ?[T](e: =>Event[T]) = new EventNodeCond(e)

}

// vim: set ts=2 sw=2 et:
