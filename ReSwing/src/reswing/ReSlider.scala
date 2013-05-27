package reswing

import java.awt.Dimension

import scala.swing.Slider
import scala.swing.event.ValueChanged

class ReSlider(
    paintLabels: OptionalArgument[Boolean],
    paintTicks: OptionalArgument[Boolean],
    paintTrack: OptionalArgument[Boolean],
    snapToTicks: OptionalArgument[Boolean],
    minorTickSpacing: OptionalArgument[Int],
    majorTickSpacing: OptionalArgument[Int]) extends ReComponent {
  override protected lazy val peer = new Slider with SliderMixin
  
  for (v <- paintLabels) peer.paintLabels = v
  for (v <- paintTicks) peer.paintTicks = v
  for (v <- paintTrack) peer.paintTrack = v
  for (v <- snapToTicks) peer.snapToTicks = v
  for (v <- minorTickSpacing) peer.minorTickSpacing = v
  for (v <- majorTickSpacing) peer.majorTickSpacing = v
  
  protected trait SliderMixin extends Slider with ComponentMixin {
    override def min_=(v: Int) = Macros.defaultSetterOverride
    override def max_=(v: Int) = Macros.defaultSetterOverride
    override def value_=(v: Int) = Macros.defaultSetterOverride
    override def extent_=(v: Int) = Macros.defaultSetterOverride
  }
  
  lazy val min = ImperativeSignal.noSignal[Int]
  lazy val max = ImperativeSignal.noSignal[Int]
  lazy val value = ImperativeSignal.noSignal[Int]
  lazy val extent = ImperativeSignal.noSignal[Int]
  
  connectSignal(min, peer.min, peer.min_=)
  connectSignal(max, peer.max, peer.max_=)
  connectSignal(value, peer.value, peer.value_=)
  connectSignal(extent, peer.extent, peer.extent_=)
  
  peer.reactions += {
    case e @ ValueChanged(_) => value(peer.value)
  }
}

object ReSlider {
  implicit def toLabel(input : ReSlider) : Slider = input.peer
  
  def apply(
      paintLabels: OptionalArgument[Boolean] = NoArgument,
      paintTicks: OptionalArgument[Boolean] = NoArgument,
      paintTrack: OptionalArgument[Boolean] = NoArgument,
      snapToTicks: OptionalArgument[Boolean] = NoArgument,
      minorTickSpacing: OptionalArgument[Int] = NoArgument,
      majorTickSpacing: OptionalArgument[Int] = NoArgument,
      min: ImperativeSignal[Int] = ImperativeSignal.noSignal,
      max: ImperativeSignal[Int] = ImperativeSignal.noSignal,
      value: ImperativeSignal[Int] = ImperativeSignal.noSignal,
      extent: ImperativeSignal[Int] = ImperativeSignal.noSignal,
      minimumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      maximumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      preferredSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      enabled: ImperativeSignal[Boolean] = ImperativeSignal.noSignal) =
        Macros.defaultObjectCreation
}
