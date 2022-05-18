package reswing

import javax.swing.Icon

import scala.swing.{AbstractButton, Alignment, Color, Dimension, Font}
import scala.swing.event.ButtonClicked

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
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends ReComponent(background, foreground, font, enabled, minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer: AbstractButton with ComponentMixin = new AbstractButton with ComponentMixin

  text.using({ () => peer.text }, peer.text_= _, "text")
  selected.using({ () => peer.selected }, peer.selected_= _, classOf[ButtonClicked])

  horizontalAlignment.using({ () => peer.horizontalAlignment }, peer.horizontalAlignment = _, "horizontalAlignment")
  verticalAlignment.using({ () => peer.verticalAlignment }, peer.verticalAlignment = _, "verticalAlignment")
  horizontalTextPosition.using(
    { () => peer.horizontalTextPosition },
    peer.horizontalTextPosition = _,
    "horizontalTextPosition"
  )
  verticalTextPosition.using({ () => peer.verticalTextPosition }, peer.verticalTextPosition_= _, "verticalTextPosition")

  icon.using({ () => peer.icon }, peer.icon_= _, "icon")
  pressedIcon.using({ () => peer.pressedIcon }, peer.pressedIcon_= _, "pressedIcon")
  selectedIcon.using({ () => peer.selectedIcon }, peer.selectedIcon_= _, "selectedIcon")
  disabledIcon.using({ () => peer.disabledIcon }, peer.disabledIcon_= _, "disabledIcon")
  disabledSelectedIcon.using({ () => peer.disabledSelectedIcon }, peer.disabledSelectedIcon_= _, "disabledSelectedIcon")
  rolloverIcon.using({ () => peer.rolloverIcon }, peer.rolloverIcon_= _, "rolloverIcon")
  rolloverSelectedIcon.using({ () => peer.rolloverSelectedIcon }, peer.rolloverSelectedIcon_= _, "rolloverSelectedIcon")

  val clicked = ReSwingEvent.using(classOf[ButtonClicked])
}

object ReAbstractButton {
  implicit def toAbstractButton(component: ReAbstractButton): AbstractButton = component.peer
}
