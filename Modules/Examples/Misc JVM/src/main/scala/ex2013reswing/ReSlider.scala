package ex2013reswing

import scala.swing.event.ValueChanged
import scala.swing.{Color, Dimension, Font, Label, Slider}

@scala.annotation.nowarn("msg=shadows field")
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
  override protected lazy val peer: Slider & ComponentMixin = new Slider with ComponentMixin

  min.using({ () => peer.min }, peer.min_=, "minimum")
  max.using({ () => peer.max }, peer.max_=, "maximum")
  value.using({ () => peer.value }, peer.value_=, classOf[ValueChanged])
  extent.using({ () => peer.extent }, peer.extent_=)

  paintLabels.using({ () => peer.paintLabels }, peer.paintLabels_=, "paintLabels")
  paintTicks.using({ () => peer.paintTicks }, peer.paintTicks_=, "paintTicks")
  paintTrack.using({ () => peer.paintTrack }, peer.paintTrack_=, "paintTrack")

  snapToTicks.using({ () => peer.snapToTicks }, peer.snapToTicks_=, "snapToTicks")

  minorTickSpacing.using({ () => peer.minorTickSpacing }, peer.minorTickSpacing_=, "minorTickSpacing")
  majorTickSpacing.using({ () => peer.majorTickSpacing }, peer.majorTickSpacing_=, "majorTickSpacing")

  labels.using(
    { () => if peer.peer.getLabelTable != null then peer.labels.toMap else null },
    peer.labels_=,
    "labelTable"
  )
}

object ReSlider {
  implicit def toSlider(component: ReSlider): Slider = component.peer
}
