package reactex

import react.events.ImperativeEvent

class Observable[T, U](body: T => U) extends (T => U) {
  // before and after, modeled as primitive events
  lazy val before = new ImperativeEvent[T]
  lazy val after = new ImperativeEvent[(T,U)]

  /*
   * Instrumented method implementation:
   * trigger events before and after the actual method execution
   */
  def apply(t: T): U = {
    before(t)
    val res = body(t)
    after(t, res)
    res
  }
}

object Observable {
  def apply[T,U](f: T => U) = new Observable(f)
}
