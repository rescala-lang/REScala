package ex2013reswing

import javax.swing.Icon
import scala.annotation.nowarn
import scala.swing.{Action, Alignment, Button, Color, Dimension, Font}

@nowarn("msg=shadows field")
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
    preferredSize: ReSwingValue[Dimension] = ()
) extends ReAbstractButton(
      text,
      selected,
      horizontalAlignment,
      verticalAlignment,
      horizontalTextPosition,
      verticalTextPosition,
      icon,
      pressedIcon,
      selectedIcon,
      disabledIcon,
      disabledSelectedIcon,
      rolloverIcon,
      rolloverSelectedIcon,
      background,
      foreground,
      font,
      enabled,
      minimumSize,
      maximumSize,
      preferredSize
    ) {
  override protected lazy val peer: Button & ComponentMixin = new Button with ComponentMixin

  if action != null then
    peer.action = action
}

object ReButton {
  implicit def toButton(component: ReButton): Button = component.peer
}
