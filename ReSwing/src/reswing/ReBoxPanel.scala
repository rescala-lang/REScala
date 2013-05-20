package reswing

import java.awt.Dimension

import scala.swing.BoxPanel
import scala.swing.Component
import scala.swing.Orientation

class ReBoxPanel(orientation: Orientation.Value) extends RePanel with ReSequentialContainer {
  override protected lazy val peer = new BoxPanel(orientation) with ComponentMixin
}

object ReBoxPanel {
  implicit def toLabel(input : ReBoxPanel) : BoxPanel = input.peer
  
  def apply(
      orientation: Orientation.Value,
      contents: ImperativeSignal[Seq[Component]] = ImperativeSignal.noSignal,
      minimumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      maximumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      preferredSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal) = Macros.applyBody
}
