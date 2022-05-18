package reswing

import scala.swing.{Component, Dimension, MainFrame, Point, Rectangle}

class ReMainFrame(
    contents: ReSwingValue[Component] = (),
    title: ReSwingValue[String] = (),
    size: ReSwingValue[Dimension] = (),
    location: ReSwingValue[Point] = (),
    bounds: ReSwingValue[Rectangle] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends ReFrame(contents, title, size, location, bounds, minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer: MainFrame = new MainFrame
}

object ReMainFrame {
  implicit def toMainFrame(component: ReMainFrame): MainFrame = component.peer
}
