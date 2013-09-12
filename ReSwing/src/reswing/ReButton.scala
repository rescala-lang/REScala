package reswing

import scala.language.implicitConversions
import scala.swing.Action
import scala.swing.Button
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font

class ReButton(
    text: ReSwingValue[String] = ReSwingValue.noValue,
    action: Action = null,
    background: ReSwingValue[Color] = ReSwingValue.noValue,
    foreground: ReSwingValue[Color] = ReSwingValue.noValue,
    font: ReSwingValue[Font] = ReSwingValue.noValue,
    enabled: ReSwingValue[Boolean] = ReSwingValue.noValue,
    minimumSize: ReSwingValue[Dimension] = ReSwingValue.noValue,
    maximumSize: ReSwingValue[Dimension] = ReSwingValue.noValue,
    preferredSize: ReSwingValue[Dimension] = ReSwingValue.noValue)
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
