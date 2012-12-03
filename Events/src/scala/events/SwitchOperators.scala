package scala.events

abstract class IntervalEventFilter {
  self =>
  object Negation extends IntervalEventFilter {
    protected[this] val ie = self.ie
    def apply() = !self.apply()
  }
  protected[this] val ie: IntervalEvent[Any,Any]
  protected[events] def deploy = ie.deploy
  protected[events] def undeploy = ie.undeploy
  def apply(): Boolean
  def unary_!() = Negation
}

class WithinEvent[T,U](protected[this] val ie: IntervalEvent[T,U]) extends IntervalEventFilter {
  def apply() = ie.active
}

class FromEvent[T](val start: Event[T]) extends IntervalEvent[T,Nothing] {

  val end = emptyevent

  protected[this] def onStart(t: T) {
  }

  protected[this] def onEnd(u: Nothing) {
  }
}


// vim: set ts=2 sw=2 et:
