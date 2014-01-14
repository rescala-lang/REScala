package reswing

import scala.language.implicitConversions
import scala.swing.Dimension
import scala.swing.Point
import scala.swing.Rectangle
import scala.swing.UIElement
import scala.swing.event.UIElementMoved
import scala.swing.event.UIElementResized

abstract class ReUIElement(
    val minimumSize: ReSwingValue[Dimension] = (),
    val maximumSize: ReSwingValue[Dimension] = (),
    val preferredSize: ReSwingValue[Dimension] = ())
  extends
    ReSwingValueConnection with ReSwingEventConnection {
  protected def peer: UIElement
  
  val size = ReSwingValue using (peer.size _, classOf[UIElementResized])
  val location = ReSwingValue using (peer.location _, classOf[UIElementMoved])
  val bounds = ReSwingValue using (peer.bounds _, classOf[UIElementResized],
                                                  classOf[UIElementMoved])
  
  minimumSize using (peer.minimumSize _, peer.minimumSize_= _, "minimumSize")
  maximumSize using (peer.maximumSize _, peer.maximumSize_= _, "maximumSize")
  preferredSize using (peer.preferredSize _, peer.preferredSize_= _, "preferredSize")
}

object ReUIElement {
  implicit def toUIElement(component: ReUIElement): UIElement = component.peer
}
