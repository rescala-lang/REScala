package reswing.reshapes.ui.panels

import reactives.default.*
import reswing.ReLabel
import reswing.reshapes.ReShapes

import scala.swing.FlowPanel

/** Small info panel which displays information like how many shapes are drawn
  * or which shape is currently selected
  */
class InfoPanel extends FlowPanel {
  def state = ReShapes.drawingSpaceState

  val shapeCount = Signal.dynamic { // #SIG
    if state.value != null then "#elements: %d" format state.value.shapes.value.size else ""
  }

  val color = Signal.dynamic { // #SIG
    if state.value != null then
      "color: %d-%d-%d".format(
        state.value.color.value.getRed,
        state.value.color.value.getGreen,
        state.value.color.value.getBlue
      )
    else ""
  }

  val strokeWidth = Signal.dynamic { // #SIG
    if state.value != null then "stroke width: %d" format state.value.strokeWidth.value else ""
  }

  val nextShape = Signal.dynamic { // #SIG
    if state.value != null then "next shape: %s" format state.value.nextShape.value.toString else ""
  }

  val selectedShape = Signal.dynamic { // #SIG
    if state.value != null && state.value.selectedShape.value != null then
      "selected: %s".format(state.value.selectedShape.value.toString)
    else ""
  }

  contents += new ReLabel(Signal.dynamic { // #SIG //#IS( //)
    "%s | %s | %s | %s | %s".format(
      shapeCount.value,
      color.value,
      strokeWidth.value,
      nextShape.value,
      selectedShape.value
    )
  })
}
