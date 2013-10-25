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
  
  val size: ReSwingValue[Dimension] = ()
  val location: ReSwingValue[Point] = ()
  val bounds: ReSwingValue[Rectangle] = ()
  
  protected def initSizeValues {
    size using (peer.size _, (peer, classOf[UIElementResized]))
    location using (peer.location _, (peer, classOf[UIElementMoved]))
    bounds using (peer.bounds _, (peer, classOf[UIElementResized]),
                                 (peer, classOf[UIElementMoved]))
  }
  initSizeValues
  
  minimumSize using (peer.minimumSize _, peer.minimumSize_= _, "minimumSize")
  maximumSize using (peer.maximumSize _, peer.maximumSize_= _, "maximumSize")
  preferredSize using (peer.preferredSize _, peer.preferredSize_= _, "preferredSize")
}

object ReUIElement {
  implicit def toUIElement(component: ReUIElement): UIElement = component.peer
}
