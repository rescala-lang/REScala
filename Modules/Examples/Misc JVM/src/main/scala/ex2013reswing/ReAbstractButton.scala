package ex2013reswing

import javax.swing.Icon
import scala.swing.event.ButtonClicked
import scala.swing.{AbstractButton, Alignment, Color, Dimension, Font}

class ReAbstractButton(
    val text: ReSwingValue[String] = (),
    val selected: ReSwingValue[Boolean] = (),
    val horizontalAlignment: ReSwingValue[Alignment.Value] = (),
    val verticalAlignment: ReSwingValue[Alignment.Value] = (),
    val horizontalTextPosition: ReSwingValue[Alignment.Value] = (),
    val verticalTextPosition: ReSwingValue[Alignment.Value] = (),
    val icon: ReSwingValue[Icon] = (),
    val pressedIcon: ReSwingValue[Icon] = (),
    val selectedIcon: ReSwingValue[Icon] = (),
    val disabledIcon: ReSwingValue[Icon] = (),
    val disabledSelectedIcon: ReSwingValue[Icon] = (),
    val rolloverIcon: ReSwingValue[Icon] = (),
    val rolloverSelectedIcon: ReSwingValue[Icon] = (),
    background1: ReSwingValue[Color] = (),
    foreground1: ReSwingValue[Color] = (),
    font1: ReSwingValue[Font] = (),
    enabled1: ReSwingValue[Boolean] = (),
    minimumSize1: ReSwingValue[Dimension] = (),
    maximumSize1: ReSwingValue[Dimension] = (),
    preferredSize1: ReSwingValue[Dimension] = ()
) extends ReComponent(background1, foreground1, font1, enabled1, minimumSize1, maximumSize1, preferredSize1) {
  override protected lazy val peer: AbstractButton & ComponentMixin = new AbstractButton with ComponentMixin

  text.using({ () => peer.text }, peer.text_=, "text")
  selected.using({ () => peer.selected }, peer.selected_=, classOf[ButtonClicked])

  horizontalAlignment.using({ () => peer.horizontalAlignment }, peer.horizontalAlignment = _, "horizontalAlignment")
  verticalAlignment.using({ () => peer.verticalAlignment }, peer.verticalAlignment = _, "verticalAlignment")
  horizontalTextPosition.using(
    { () => peer.horizontalTextPosition },
    peer.horizontalTextPosition = _,
    "horizontalTextPosition"
  )
  verticalTextPosition.using({ () => peer.verticalTextPosition }, peer.verticalTextPosition_=, "verticalTextPosition")

  icon.using({ () => peer.icon }, peer.icon_=, "icon")
  pressedIcon.using({ () => peer.pressedIcon }, peer.pressedIcon_=, "pressedIcon")
  selectedIcon.using({ () => peer.selectedIcon }, peer.selectedIcon_=, "selectedIcon")
  disabledIcon.using({ () => peer.disabledIcon }, peer.disabledIcon_=, "disabledIcon")
  disabledSelectedIcon.using({ () => peer.disabledSelectedIcon }, peer.disabledSelectedIcon_=, "disabledSelectedIcon")
  rolloverIcon.using({ () => peer.rolloverIcon }, peer.rolloverIcon_=, "rolloverIcon")
  rolloverSelectedIcon.using({ () => peer.rolloverSelectedIcon }, peer.rolloverSelectedIcon_=, "rolloverSelectedIcon")

  val clicked = ReSwingEvent.using(classOf[ButtonClicked])
}

object ReAbstractButton {
  implicit def toAbstractButton(component: ReAbstractButton): AbstractButton = component.peer
}
