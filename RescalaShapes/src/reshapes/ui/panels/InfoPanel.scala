package reshapes.ui.panels
import scala.events.ImperativeEvent
import reshapes.figures.Shape
import scala.swing._
import java.awt.Color
import reshapes.Events
import scala.events.behaviour.Signal
import reshapes.Reshapes

/**
 * Small info panel which displays infos like how many shapes are drawn or which shape is currently selected.
 */
class InfoPanel(var _events: Events) extends FlowPanel {

  var signal = Signal { println(Reshapes.CurrentEvents.allShapes().size) }

  def events = _events
  def events_=(e: Events) {
    _events.nextShape.changed -= updateCurrentShapeLabel
    _events.selectedShape.changed -= updateCurrentShapeLabel
    _events.allShapes.changed -= updateNumberElementsLabel
    _events.modeChange -= updateModeLabel
    _events = e
    _events.nextShape.changed += updateCurrentShapeLabel
    _events.selectedShape.changed += updateCurrentShapeLabel
    _events.allShapes.changed += updateNumberElementsLabel
    _events.modeChange += updateModeLabel
  }

  val currentShapeLabel = new Label { text = " " }
  val numberElementsLabel = new Label { text = "#elements: 0" }
  val modeLabel = new Label { text = "mode: " }

  contents += currentShapeLabel
  contents += new Label { text = "|" }
  contents += numberElementsLabel
  contents += new Label { text = "|" }
  contents += modeLabel

  def updateCurrentShapeLabel(shape: Shape) = {
    currentShapeLabel.text = if (shape != null) shape.toString() else "";
  }

  def updateNumberElementsLabel(shapes: List[Shape]) = {
    numberElementsLabel.text = "#elements: %d".format(shapes.size)
  }

  def updateModeLabel(x: Any) = {
    modeLabel.text = "mode : %s".format(events.mode.getClass().getSimpleName())
  }
}