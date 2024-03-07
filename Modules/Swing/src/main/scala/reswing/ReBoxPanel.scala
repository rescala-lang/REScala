package reswing

import scala.swing.{BoxPanel, Color, Dimension, Font, Orientation}

class ReBoxPanel(
    orientation: Orientation.Value,
    val contents: ReSwingValue[CompList] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends RePanel(background, foreground, font, enabled, minimumSize, maximumSize, preferredSize)
    with ReSequentialContainer {
  override protected lazy val peer: BoxPanel & ComponentMixin = new BoxPanel(orientation) with ComponentMixin
}

object ReBoxPanel {
  implicit def toBoxPanel(component: ReBoxPanel): BoxPanel = component.peer
}
