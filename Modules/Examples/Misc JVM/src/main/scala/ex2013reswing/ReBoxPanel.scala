package ex2013reswing

import scala.swing.{BoxPanel, Color, Dimension, Font, Orientation}

class ReBoxPanel(
    orientation: Orientation.Value,
    val contents: ReSwingValue[CompList] = (),
    background1: ReSwingValue[Color] = (),
    foreground1: ReSwingValue[Color] = (),
    font1: ReSwingValue[Font] = (),
    enabled1: ReSwingValue[Boolean] = (),
    minimumSize1: ReSwingValue[Dimension] = (),
    maximumSize1: ReSwingValue[Dimension] = (),
    preferredSize1: ReSwingValue[Dimension] = ()
) extends RePanel(background1, foreground1, font1, enabled1, minimumSize1, maximumSize1, preferredSize1)
    with ReSequentialContainer {
  override protected lazy val peer: BoxPanel & ComponentMixin = new BoxPanel(orientation) with ComponentMixin
}

object ReBoxPanel {
  implicit def toBoxPanel(component: ReBoxPanel): BoxPanel = component.peer
}
