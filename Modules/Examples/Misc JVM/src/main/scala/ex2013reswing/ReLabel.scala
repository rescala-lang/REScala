package ex2013reswing

import javax.swing.Icon
import scala.annotation.nowarn
import scala.swing.{Alignment, Color, Dimension, Font, Label}

@nowarn("msg=shadows field")
class ReLabel(
    val text: ReSwingValue[String] = (),
    val horizontalAlignment: ReSwingValue[Alignment.Value] = (),
    val verticalAlignment: ReSwingValue[Alignment.Value] = (),
    val horizontalTextPosition: ReSwingValue[Alignment.Value] = (),
    val verticalTextPosition: ReSwingValue[Alignment.Value] = (),
    val icon: ReSwingValue[Icon] = (),
    val disabledIcon: ReSwingValue[Icon] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends ReComponent(background, foreground, font, enabled, minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer: Label & ComponentMixin = new Label with ComponentMixin

  text.using({ () => peer.text }, peer.text_=, "text")

  horizontalAlignment.using({ () => peer.horizontalAlignment }, peer.horizontalAlignment = _, "horizontalAlignment")
  verticalAlignment.using({ () => peer.verticalAlignment }, peer.verticalAlignment = _, "verticalAlignment")
  horizontalTextPosition.using(
    { () => peer.horizontalTextPosition },
    peer.horizontalTextPosition = _,
    "horizontalTextPosition"
  )
  verticalTextPosition.using({ () => peer.verticalTextPosition }, peer.verticalTextPosition_=, "verticalTextPosition")

  icon.using({ () => peer.icon }, peer.icon_=, "icon")
  disabledIcon.using({ () => peer.disabledIcon }, peer.disabledIcon_=, "disabledIcon")
}

object ReLabel {
  implicit def toLabel(component: ReLabel): Label = component.peer
}
