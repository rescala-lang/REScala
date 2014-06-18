package reswing

import scala.language.implicitConversions
import scala.swing.Alignment
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.MenuBar

import javax.swing.Icon

class ReMenuBar(
    val contents: ReSwingValue[CompList] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ())
  extends
    ReComponent(background, foreground, font, enabled,
                minimumSize, maximumSize, preferredSize)
  with
    ReSequentialContainer {
  override protected lazy val peer = new MenuBar with ComponentMixin
}

object ReMenuBar {
  implicit def toMenuBar(component: ReMenuBar): MenuBar = component.peer
}
