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
  
  val windowActivated = event using (peer, classOf[WindowActivated])
  val windowClosed = event using (peer, classOf[WindowClosed])
  val windowClosing = event using (peer, classOf[WindowClosing])
  val windowDeactivated = event using (peer, classOf[WindowDeactivated])
  val windowDeiconified = event using (peer, classOf[WindowDeiconified])
  val windowIconified = event using (peer, classOf[WindowIconified])
  val windowOpened = event using (peer, classOf[WindowOpened])
}

object ReWindow {
  implicit def toWindow(component: ReWindow): Window = component.peer
}
