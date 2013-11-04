package reswing

import scala.swing.Publisher
import scala.swing.Reactor
import scala.swing.UIElement
import react.events.Event

/**
 * Introduces methods to connect a [[ReSwingEvent]] to the corresponding
 * `Swing` reaction of the underlying `Swing` component.
 * 
 * Classes that extend this trait can use a declarative syntax to specify
 * the connections between a [[ReSwingEvent]] and the `Swing` event, e.g.:
 * 
 * {{{
 * abstract class ReWindow
 * extends ReSwingEventConnection {
 *   protected def peer: Window
 *   
 *   val windowActivated = ReSwingEvent using classOf[WindowActivated]
 * }
 * }}}
 * 
 * Also introduces methods to connect a [[react.events.Event]] to the
 * underlying `Swing` component that will execute a specific method whenever
 * the event fires:
 * 
 * {{{
 * abstract class ReUIElement(
 *   repaint: Event[Unit] = ())
 * extends ReSwingEventConnection {
 *   protected def peer: UIElement
 *   
 *   repaint using peer.repaint _
 * }
 * }}}
 */
private[reswing] abstract trait ReSwingEventConnection {
  protected def peer: UIElement
  
  final protected implicit class EventConnector[T] private[ReSwingEventConnection]
      (value: Event[T]) {
    def using(setter: T => Unit): Event[T] = {
      if (value != null) 
        value += { v => inSyncEDT { setter(v) } }
      value
    }
    def using(setter: () => Unit): Event[T] = {
      if (value != null) 
        value += { _ => inSyncEDT { setter() } }
      value
    }
  }
  
  protected object ReSwingEvent {
    def using[T](reaction: Class[T]): ReSwingEvent[T] =
      using(peer, reaction)
    
    def using[T](publisher: Publisher, reaction: Class[T]): ReSwingEvent[T]  = {
      val event = new ReSwingEvent[T]({ event =>
        inSyncEDT {
          reactor listenTo publisher
          reactor.reactions += { case e =>
            if (reaction isInstance e)
              event(e.asInstanceOf[T])
          }
        }
      })
      event
    }
  }
  
  private val reactor = new Reactor { }
}
