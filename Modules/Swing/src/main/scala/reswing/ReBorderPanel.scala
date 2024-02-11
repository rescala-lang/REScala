package reswing

import scala.swing.BorderPanel.Position
import scala.swing.{BorderPanel, Color, Component, Dimension, Font}

class ReBorderPanel(
    val contents: ReSwingValue[Map[Component, Position.Value]] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends RePanel(background, foreground, font, enabled, minimumSize, maximumSize, preferredSize)
    with ReLayoutContainer[Position.Value] {
  override protected lazy val peer: BorderPanel & ComponentMixin = new BorderPanel with ComponentMixin
}

object ReBorderPanel {
  implicit def toBorderPanel(component: ReBorderPanel): BorderPanel = component.peer
}
