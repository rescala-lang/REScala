package reswing

import scala.language.implicitConversions
import scala.swing.Action
import scala.swing.Alignment
import scala.swing.Button
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font

import javax.swing.Icon

class ReButton(
    text: ReSwingValue[String] = (),
    action: Action = null,
    selected: ReSwingValue[Boolean] = (),
    horizontalAlignment: ReSwingValue[Alignment.Value] = (),
    verticalAlignment: ReSwingValue[Alignment.Value] = (),
    horizontalTextPosition: ReSwingValue[Alignment.Value] = (),
    verticalTextPosition: ReSwingValue[Alignment.Value] = (),
    icon: ReSwingValue[Icon] = (),
    pressedIcon: ReSwingValue[Icon] = (),
    selectedIcon: ReSwingValue[Icon] = ReSwingValue.apply(()),
    disabledIcon: ReSwingValue[Icon] = (),
    disabledSelectedIcon: ReSwingValue[Icon] = (),
    rolloverIcon: ReSwingValue[Icon] = (),
    rolloverSelectedIcon: ReSwingValue[Icon] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ())
  extends
    ReAbstractButton(text, selected, horizontalAlignment, verticalAlignment,
                     horizontalTextPosition, verticalTextPosition,
                     icon, pressedIcon, selectedIcon, disabledIcon,
                     disabledSelectedIcon, rolloverIcon, rolloverSelectedIcon,
                     background, foreground, font, enabled,
                     minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer = new Button with ComponentMixin

  if (action != null)
    peer.action = action
}

object ReButton {
  implicit def toButton(component: ReButton): Button = component.peer
}
