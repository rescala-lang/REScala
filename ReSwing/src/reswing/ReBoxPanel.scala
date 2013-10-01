package reswing

import scala.language.implicitConversions
import scala.swing.BoxPanel
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.Orientation

class ReBoxPanel(
    orientation: Orientation.Value,
    val contents: ReSwingValue[CompList] = (),
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
    ReSequentialContainer {
  override protected lazy val peer = new BoxPanel(orientation) with ComponentMixin
}

object ReBoxPanel {
  implicit def toComponent(component: ReBoxPanel): BoxPanel = component.peer
}
