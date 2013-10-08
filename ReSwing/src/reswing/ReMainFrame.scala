package reswing

import scala.language.implicitConversions
import scala.swing.Component
import scala.swing.Dimension
import scala.swing.MainFrame
import scala.swing.Point
import scala.swing.Rectangle

class ReMainFrame(
    contents: ReSwingValue[Component] = (),
    title: ReSwingValue[String] = (),
    size: ReSwingValue[Dimension] = (),
    location: ReSwingValue[Point] = (),
    bounds: ReSwingValue[Rectangle] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ())
  extends
    ReFrame(contents, title, size, location, bounds,
            minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer = new MainFrame
}

object ReMainFrame {
  implicit def toMainFrame(component: ReMainFrame): MainFrame = component.peer
}
