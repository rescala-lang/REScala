package ex2013reswing

import javax.swing.Icon
import scala.swing.{Action, Alignment, Color, Dimension, Font, Menu}

@scala.annotation.nowarn("msg=shadows field")
class ReMenu(
    text: ReSwingValue[String] = (),
    action: Action = null,
    val contents: ReSwingValue[CompList] = (),
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
) extends ReMenuItem(
      text,
      action,
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
    )
    with ReSequentialContainer {
  override protected lazy val peer: Menu & ComponentMixin = new Menu(null) with ComponentMixin
}

object ReMenu {
  implicit def toMenu(component: ReMenu): Menu = component.peer
}
