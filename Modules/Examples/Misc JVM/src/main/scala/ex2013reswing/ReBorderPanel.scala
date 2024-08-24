package ex2013reswing

import scala.swing.BorderPanel.Position
import scala.swing.{BorderPanel, Color, Component, Dimension, Font}

class ReBorderPanel(
    val contents: ReSwingValue[Map[Component, Position.Value]] = (),
    background1: ReSwingValue[Color] = (),
    foreground1: ReSwingValue[Color] = (),
    font1: ReSwingValue[Font] = (),
    enabled1: ReSwingValue[Boolean] = (),
    minimumSize1: ReSwingValue[Dimension] = (),
    maximumSize1: ReSwingValue[Dimension] = (),
    preferredSize1: ReSwingValue[Dimension] = ()
) extends RePanel(background1, foreground1, font1, enabled1, minimumSize1, maximumSize1, preferredSize1)
    with ReLayoutContainer[Position.Value] {
  override protected lazy val peer: BorderPanel & ComponentMixin = new BorderPanel with ComponentMixin
}

object ReBorderPanel {
  implicit def toBorderPanel(component: ReBorderPanel): BorderPanel = component.peer
}
