package reshapes.ui.panels

import scala.swing.FlowPanel

import macro.SignalMacro.{SignalM => Signal}
import react.SignalSynt
import reshapes.ReShapes
import reswing.ReLabel

/**
 * Small info panel which displays information like how many shapes are drawn
 * or which shape is currently selected
 */
class InfoPanel extends FlowPanel {
  def state = ReShapes.drawingSpaceState
  
  val shapeCount = Signal {
    if (state() != null) "#elements: %d" format state().shapes().size else "" }
  
  val color = Signal {
    if (state() != null)
      "color: %d-%d-%d" format
        (state().color().getRed, state().color().getGreen, state().color().getBlue)
    else ""
  }
  
  val strokeWidth = Signal {
    if (state() != null) "stroke width: %d" format state().strokeWidth() else "" }
  
  val nextShape = Signal {
    if (state() != null) "next shape: %s" format state().nextShape().toString else "" }
  
  val selectedShape = Signal {
    if (state() != null && state().selectedShape() != null)
      "selected: %s" format state().selectedShape().toString
    else ""
  }
  
  contents += ReLabel(Signal {
    "%s | %s | %s | %s | %s" format
      (shapeCount(), color(), strokeWidth(), nextShape(), selectedShape()) } )
}
