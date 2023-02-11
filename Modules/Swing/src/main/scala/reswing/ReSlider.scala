package reswing

import scala.swing.{Color, Dimension, Font, Label, Slider}
import scala.swing.event.ValueChanged

class ReSlider(
    val min: ReSwingValue[Int] = (),
    val max: ReSwingValue[Int] = (),
    val value: ReSwingValue[Int] = (),
    val extent: ReSwingValue[Int] = (),
    val paintLabels: ReSwingValue[Boolean] = (),
    val paintTicks: ReSwingValue[Boolean] = (),
    val paintTrack: ReSwingValue[Boolean] = (),
    val snapToTicks: ReSwingValue[Boolean] = (),
    val minorTickSpacing: ReSwingValue[Int] = (),
    val majorTickSpacing: ReSwingValue[Int] = (),
    val labels: ReSwingValue[Map[Int, Label]] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends ReComponent(background, foreground, font, enabled, minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer: Slider with ComponentMixin = new Slider with ComponentMixin

  min.using({ () => peer.min }, peer.min_= _, "minimum")
  max.using({ () => peer.max }, peer.max_= _, "maximum")
  value.using({ () => peer.value }, peer.value_= _, classOf[ValueChanged])
  extent.using({ () => peer.extent }, peer.extent_= _)

  paintLabels.using({ () => peer.paintLabels }, peer.paintLabels_= _, "paintLabels")
  paintTicks.using({ () => peer.paintTicks }, peer.paintTicks_= _, "paintTicks")
  paintTrack.using({ () => peer.paintTrack }, peer.paintTrack_= _, "paintTrack")

  snapToTicks.using({ () => peer.snapToTicks }, peer.snapToTicks_= _, "snapToTicks")

  minorTickSpacing.using({ () => peer.minorTickSpacing }, peer.minorTickSpacing_= _, "minorTickSpacing")
  majorTickSpacing.using({ () => peer.majorTickSpacing }, peer.majorTickSpacing_= _, "majorTickSpacing")

  labels.using(
    { () => if (peer.peer.getLabelTable != null) peer.labels.toMap else null },
    peer.labels_= _,
    "labelTable"
  )
}

object ReSlider {
  implicit def toSlider(component: ReSlider): Slider = component.peer
}
