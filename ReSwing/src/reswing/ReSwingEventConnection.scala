package reswing

import scala.swing.Publisher
import scala.swing.Reactor
import scala.swing.UIElement
import scala.swing.event.Event

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
 */
private[reswing] abstract trait ReSwingEventConnection {
  protected def peer: UIElement
  
  protected object ReSwingEvent {
    def using[T](reaction: Class[T]): ReSwingEvent[T] =
      using(peer, reaction)
    
    def using[T](publisher: Publisher, reaction: Class[T]): ReSwingEvent[T]  = {
      val event: ReSwingEvent[T] = new ReSwingEvent[T]({ event =>
        reactor listenTo publisher
        reactor.reactions += { case e: Event =>
          if (reaction isInstance e)
            event(e.asInstanceOf[T])
        }
      })
      event
    }
    
    private val reactor = new Reactor { }
  }
}
