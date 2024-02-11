package reswing.reshapes.util

import rescala.default._
import scala.collection.mutable.ListBuffer

object ReactiveUtil {
  class BilateralValue {

    private val events = ListBuffer[() => Unit]()

    private[ReactiveUtil] def applyConnections() = events foreach (_())

    def apply[T](e: => Event[T]): Event[T] = {
      val ev = Evt[T]()
      events += { () =>
        e.observe(ev.fire); ()
      }
      ev
    }

    def apply[T](s: => Signal[T], init: T = null.asInstanceOf[T]): Signal[T] = {
      val ev = Evt[T]()
      events += { () =>
        s.observe(ev.fire)
        ev.fire(s.now)
      }
      ev `hold` init
    }
  }

  /** Enables two or more objects that are using event streams from each other
    * to establish a connection of these streams.
    * This solves a problem that arises in situations of the following kind:
    *
    * {{{
    * lazy val o1: { val ev: Event[Unit] } = new {
    *   lazy val ev: Event[Unit] = Evt[Unit]()
    *   o2.ev observe {_ => /* react on event */ }
    * }
    *
    * lazy val o2: { val ev: Event[Unit] } = new {
    *   lazy val ev: Event[Unit] = Evt[Unit]()
    *   o1.ev observe {_ => /* react on event */ }
    * }
    *
    * (o1, o2)
    * }}}
    *
    * Here both objects `o1` and `o2` depend on the event `ev` of the other
    * object (there is no cycle in the event stream itself).
    * This will cause a `StackOverflowError` error since `ev` is accessed in
    * the object constructor which needs the other object to be constructed
    * which will in turn need the first object to be constructed and so on.
    *
    * As a solution, the following code can be used:
    *
    * {{{
    * val (o1, o2) = bilateralValues{ value =>
    *   lazy val o1: { val ev: Event[Unit] } = new {
    *     lazy val ev: Event[Unit] = Evt[Unit]()
    *     value(o2.ev) observe {_ => /* react on event */ }
    *   }
    *
    *   lazy val o2: { val ev: Event[Unit] } = new {
    *     lazy val ev: Event[Unit] = Evt[Unit]()
    *     value(o1.ev) observe {_ => /* react on event */ }
    *   }
    *
    *   (o1, o2)
    * }
    * }}}
    *
    * This will construct both objects when they are returned as a pair
    * from the function passed to `bilateralValues` and will establish
    * the event stream connection after object construction has completed,
    * thus circumventing the stack overflow issue.
    */
  def bilateralValues[T <: AnyRef](body: BilateralValue => T): T = {
    val connect = new BilateralValue
    val res     = body(connect)
    connect.applyConnections()
    res
  }

  /** Creates an [[rescala.default.Event]] that fires whenever
    * any event in the given event list fires, where the list itself
    * is wrapped in a [[rescala.default.Signal]].
    */
  object UnionEvent {
    def apply[T, E[T] <: Event[T], L[E] <: Iterable[E]](signal: Signal[L[E[T]]]): Event[T] = {
      Event.dynamic {
        signal.value.flatMap(e => e.value).headOption
      }
    }
  }
}
