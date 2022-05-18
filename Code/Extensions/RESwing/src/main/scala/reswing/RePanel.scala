package reswing

import scala.swing.{Color, Dimension, Font, Panel}

class RePanel(
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends ReComponent(background, foreground, font, enabled, minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer: Panel with ComponentMixin = new Panel with ComponentMixin
}

object RePanel {
  implicit def toPanel(component: RePanel): Panel = component.peer
}
