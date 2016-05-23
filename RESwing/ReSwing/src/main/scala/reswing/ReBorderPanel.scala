package reswing 

import scala.language.implicitConversions
import scala.swing.Component
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.Orientation

class ReBorderPanel(
    val contents: ReSwingValue[Map[Component, Position.Value]] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ())
  extends
    RePanel(background, foreground, font, enabled,
            minimumSize, maximumSize, preferredSize)
  with
    ReLayoutContainer[Position.Value] {
  override protected lazy val peer = new BorderPanel with ComponentMixin
}

object ReBorderPanel {
  implicit def toBorderPanel(component: ReBorderPanel): BorderPanel = component.peer
}
