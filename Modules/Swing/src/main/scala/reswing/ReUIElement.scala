package reswing

import scala.swing.{Dimension, UIElement}
import scala.swing.event.{UIElementMoved, UIElementResized}

abstract class ReUIElement(
    val minimumSize: ReSwingValue[Dimension] = (),
    val maximumSize: ReSwingValue[Dimension] = (),
    _preferredSize: ReSwingValue[Dimension] = ()
) extends ReSwingValueConnection
    with ReSwingEventConnection {
  protected def peer: UIElement

  def preferredSize: ReSwingValue[Dimension] = _preferredSize

  val size     = ReSwingValue.using({ () => peer.size }, classOf[UIElementResized])
  val location = ReSwingValue.using({ () => peer.location }, classOf[UIElementMoved])
  val bounds   = ReSwingValue.using({ () => peer.bounds }, classOf[UIElementResized], classOf[UIElementMoved])

  minimumSize.using({ () => peer.minimumSize }, peer.minimumSize_=, "minimumSize")
  maximumSize.using({ () => peer.maximumSize }, peer.maximumSize_=, "maximumSize")
  preferredSize.using({ () => peer.preferredSize }, peer.preferredSize_=, "preferredSize")

  def initReactiveLayer(): Unit = {
    initReSwingValueConnection()
    initReSwingEventConnection()
  }
}

object ReUIElement {
  implicit def toUIElement(component: ReUIElement): UIElement = component.peer
}
