package reswing

import java.awt.event.HierarchyEvent
import java.awt.event.HierarchyListener
import scala.collection.mutable.ListBuffer
import scala.swing.Publisher
import scala.swing.Reactor
import scala.swing.UIElement

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
 *   repaint: ReSwingEvent[Unit] = ())
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
      (value: ReSwingEvent[T]) {
    def using(setter: T => Unit): ReSwingEvent[T] = {
      if (value.isInstanceOf[ReSwingEventIn[_]])
        delayedInitEvents += { _ =>
          value += { v => inSyncEDT { setter(v) } }
        }
      value
    }
    def using(setter: () => Unit): ReSwingEvent[T] = {
      if (value.isInstanceOf[ReSwingEventIn[_]])
        delayedInitEvents += { _ =>
          value += { _ => inSyncEDT { setter() } }
        }
      value
    }
  }
  
  protected object ReSwingEvent {
    def using[T](reaction: Class[T]): ReSwingEvent[T] =
      using(peer, reaction)
    
    def using[T](publisher: Publisher, reaction: Class[T]): ReSwingEvent[T]  = {
      val event = new ReSwingEventOut[T]({ event =>
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
  private val delayedInitEvents = ListBuffer.empty[Unit => Unit]
  
  peer.peer.addHierarchyListener(new HierarchyListener {
    def hierarchyChanged(e: HierarchyEvent) =
      if ((e.getChangeFlags & HierarchyEvent.DISPLAYABILITY_CHANGED) != 0) {
        initReSwingEventConnection
        peer.peer.removeHierarchyListener(this)
      }
  })
  
  protected def initReSwingEventConnection {
    for (init <- delayedInitEvents)
      init()
    delayedInitEvents.clear
  }
}
