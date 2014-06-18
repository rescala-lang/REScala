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
  
  size using (peer.size _, peer.size_= _, classOf[UIElementResized])
  location using (peer.location _, peer.location_= _, classOf[UIElementMoved])
  bounds using (peer.bounds _, peer.bounds_= _, classOf[UIElementResized],
                                                classOf[UIElementMoved])
  
  val windowActivated = ReSwingEvent using (peer, classOf[WindowActivated])
  val windowClosed = ReSwingEvent using (peer, classOf[WindowClosed])
  val windowClosing = ReSwingEvent using (peer, classOf[WindowClosing])
  val windowDeactivated = ReSwingEvent using (peer, classOf[WindowDeactivated])
  val windowDeiconified = ReSwingEvent using (peer, classOf[WindowDeiconified])
  val windowIconified = ReSwingEvent using (peer, classOf[WindowIconified])
  val windowOpened = ReSwingEvent using (peer, classOf[WindowOpened])
}

object ReWindow {
  implicit def toWindow(component: ReWindow): Window = component.peer
}
