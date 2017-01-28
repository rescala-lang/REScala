package reshapes.util

import scala.collection.mutable.ListBuffer

import rescala.Signal
import rescala.events.Event
import rescala.events.ImperativeEvent

object ReactiveUtil {
  private class BilateralValue {
    private trait Connectable extends Event[Any] {
      def connect: Unit
    }
    
    private val events = ListBuffer[Event[Any] with Connectable]()
    
    private[ReactiveUtil] def applyConnections = events foreach (_.connect)
    
    def apply[T](e: => Event[T]): Event[T] = {
      val ev = new ImperativeEvent[T] with Connectable {
        override def connect = e += apply _
      }
      events += ev
      ev
    }
    
    def apply[T](s: => Signal[T], init: T = null.asInstanceOf[T]): Signal[T] = {
      val ev = new ImperativeEvent[T] with Connectable {
        override def connect {
          s.changed += apply _
          apply(s.get)
        }
      }
      events += ev
      ev latest init
    }
  }
  
  /**
   * Enables two or more objects that are using event streams from each other
   * to establish a connection of these streams.
   * This solves a problem that arises in situations of the following kind:
   * 
   * {{{
   * lazy val o1: { val ev: Event[Unit] } = new {
   *   lazy val ev: Event[Unit] = new ImperativeEvent[Unit]
   *   o2.ev += {_ => /* react on event */ }
   * }
   * 
   * lazy val o2: { val ev: Event[Unit] } = new {
   *   lazy val ev: Event[Unit] = new ImperativeEvent[Unit]
   *   o1.ev += {_ => /* react on event */ }
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
   *     lazy val ev: Event[Unit] = new ImperativeEvent[Unit]
   *     value(o2.ev) += {_ => /* react on event */ }
   *   }
   *   
   *   lazy val o2: { val ev: Event[Unit] } = new {
   *     lazy val ev: Event[Unit] = new ImperativeEvent[Unit]
   *     value(o1.ev) += {_ => /* react on event */ }
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
    val res = body(connect)
    connect.applyConnections
    res
  }
  
  /**
   * Creates an [[scala.events.Event]] that fires whenever
   * any event in the given event list fires, where the list itself
   * is wrapped in a [[scala.events.behaviour.Signal]].
   */
  object UnionEvent {
    def apply[T, E[T] <: Event[T], L[E] <: Traversable[E]]
             (signal: Signal[L[E[T]]]): Event[T] = {
      val res = new ImperativeEvent[T]
      var events: Traversable[Event[T]] = List.empty[Event[T]]
      
      def update(list: Traversable[Event[T]]) {
        for (event <- events)
          event -= res.apply
        
        events = list
        
        for (event <- events)
          event += res.apply
      }
      
      signal.changed += update
      update(signal.get)
      res
    }
  }
}