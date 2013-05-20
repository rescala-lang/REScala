package reshapes.util

import scala.collection.mutable.ListBuffer
import scala.events.Event
import scala.events.ImperativeEvent
import scala.events.behaviour.Signal

object ReactiveUtil {
  private class BilateralEvent {
    private trait Connectable extends Event[Any] {
      def connect(): Unit
    }
    
    private val events = ListBuffer[Event[Any] with Connectable]()
    
    private[ReactiveUtil] def applyConnections = events foreach (_.connect)
    
    def apply[T](e: => Event[T]): Event[T] = {
      val ev = new ImperativeEvent[T] with Connectable {
        override def connect() = e += apply _
      }
      events += ev
      ev
    }
  }
  
  def bilateralEvents[T <: AnyRef](body: BilateralEvent => T): T = {
    val connect = new BilateralEvent
    val res = body(connect)
    connect.applyConnections
    res
  }
  
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
      update(signal.getValue)
      res
    }
  }
}