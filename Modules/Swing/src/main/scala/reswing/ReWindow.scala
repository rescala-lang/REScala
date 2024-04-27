package reswing

import scala.swing.event.*
import scala.swing.{Component, Dimension, Point, Rectangle, Window}

abstract class ReWindow(
    val contents: ReSwingValue[Component] = (),
    override val size: ReSwingValue[Dimension] = (),
    override val location: ReSwingValue[Point] = (),
    override val bounds: ReSwingValue[Rectangle] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends ReUIElement(minimumSize, maximumSize, preferredSize)
    with ReRootPanel {
  protected def peer: Window

  size.using({ () => peer.size }, peer.size_=, classOf[UIElementResized])
  location.using({ () => peer.location }, peer.location_=, classOf[UIElementMoved])
  bounds.using({ () => peer.bounds }, peer.bounds_=, classOf[UIElementResized], classOf[UIElementMoved])

  val windowActivated   = ReSwingEvent.using(peer, classOf[WindowActivated])
  val windowClosed      = ReSwingEvent.using(peer, classOf[WindowClosed])
  val windowClosing     = ReSwingEvent.using(peer, classOf[WindowClosing])
  val windowDeactivated = ReSwingEvent.using(peer, classOf[WindowDeactivated])
  val windowDeiconified = ReSwingEvent.using(peer, classOf[WindowDeiconified])
  val windowIconified   = ReSwingEvent.using(peer, classOf[WindowIconified])
  val windowOpened      = ReSwingEvent.using(peer, classOf[WindowOpened])
}

object ReWindow {
  implicit def toWindow(component: ReWindow): Window = component.peer
}
