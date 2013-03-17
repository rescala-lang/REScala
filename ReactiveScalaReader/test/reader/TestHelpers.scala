package reader

import scala.events._

object testHelpers {

  def shouldFire[A](evt: Event[A])(action: => Unit) = {
    var fired = false
    evt += { _ => fired = true }

    action

    assert(fired,"Event should have fired, but didn't")
  }

  def shouldNotFire[A](evt: Event[A])(action: => Unit) = {
    var fired = false
    evt += { _ => fired = true }

    action

    assert(!fired,"Event should NOT have fired, but it did")
  }

}
