package reswing

import scala.language.implicitConversions
import scala.swing.Action
import scala.swing.Button
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font

class ReButton(
    text: ReSwingValue[String] = (),
    action: Action = null,
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ())
  extends
    ReAbstractButton(text, background, foreground, font, enabled,
                     minimumSize, maximumSize, preferredSize) {
  
  override protected lazy val peer = new Button with ComponentMixin
  
  if (action != null)
    peer.action = action
}

object ReButton {
  implicit def toButton(component: ReButton): Button = component.peer
}
