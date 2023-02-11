package reswing

import javax.swing.Icon

import scala.swing.{Alignment, CheckBox, Color, Dimension, Font}

class ReCheckBox(
    text: ReSwingValue[String] = (),
    selected: ReSwingValue[Boolean] = (),
    horizontalAlignment: ReSwingValue[Alignment.Value] = (),
    verticalAlignment: ReSwingValue[Alignment.Value] = (),
    horizontalTextPosition: ReSwingValue[Alignment.Value] = (),
    verticalTextPosition: ReSwingValue[Alignment.Value] = (),
    icon: ReSwingValue[Icon] = (),
    pressedIcon: ReSwingValue[Icon] = (),
    selectedIcon: ReSwingValue[Icon] = (),
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
) extends ReToggleButton(
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
  override protected lazy val peer: CheckBox with ComponentMixin = new CheckBox with ComponentMixin
}

object ReCheckBox {
  implicit def toCheckBox(component: ReCheckBox): CheckBox = component.peer
}
