import scala.language.implicitConversions
import scala.swing.Component
import scala.swing.Swing
import javax.swing.SwingUtilities
import react.events.Event

package object reswing {
  type CompItem = Component
  object CompItem {
    def apply(elem: Component) = elem
  }
  
  type CompList = Seq[CompItem]
  object CompList {
    def apply(elems: CompItem*) = Seq(elems: _*)
  }
  
  def inSyncEDT(op: => Unit) =
    if (SwingUtilities.isEventDispatchThread)
      op
    else
      Swing onEDTWait op
  
  implicit def toEvent[T](value: Unit): Event[T] = null
}
