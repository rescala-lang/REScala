package reswing

import scala.language.implicitConversions
import scala.swing.Component
import scala.swing.Dimension
import scala.swing.Point
import scala.swing.Rectangle
import scala.swing.Window
import scala.swing.event.UIElementMoved
import scala.swing.event.UIElementResized
import scala.swing.event._

import reswing.ReSwingValue.toReSwingValue

abstract class ReWindow(
    val contents: ReSwingValue[Component] = (),
    override val size: ReSwingValue[Dimension] = (),
    override val location: ReSwingValue[Point] = (),
    override val bounds: ReSwingValue[Rectangle] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ())
  extends
    ReUIElement(minimumSize, maximumSize, preferredSize)
  with
    ReRootPanel {
  protected def peer: Window
  
  override protected def initSizeValues = {
    size using (peer.size _, peer.size_= _, (peer, classOf[UIElementResized]))
    location using (peer.location _, peer.location_= _, (peer, classOf[UIElementMoved]))
    bounds using (peer.bounds _, peer.bounds_= _, (peer, classOf[UIElementResized]),
                                                  (peer, classOf[UIElementMoved]))
  }
  
  val windowActivated = new ReSwingEvent[WindowActivated]
  val windowClosed = new ReSwingEvent[WindowClosed]
  val windowClosing = new ReSwingEvent[WindowClosing]
  val windowDeactivated = new ReSwingEvent[WindowDeactivated]
  val windowDeiconified = new ReSwingEvent[WindowDeiconified]
  val windowIconified = new ReSwingEvent[WindowIconified]
  val windowOpened = new ReSwingEvent[WindowOpened]
  
  peer.reactions += {
    case e @ WindowActivated(_) => windowActivated(e)
    case e @ WindowClosed(_) => windowClosed(e)
    case e @ WindowClosing(_) => windowClosing(e)
    case e @ WindowDeactivated(_) => windowDeactivated(e)
    case e @ WindowDeiconified(_) => windowDeiconified(e)
    case e @ WindowIconified(_) => windowIconified(e)
    case e @ WindowOpened(_) => windowOpened(e)
  }
}

object ReWindow {
  implicit def toWindow(component: ReWindow): Window = component.peer
}
