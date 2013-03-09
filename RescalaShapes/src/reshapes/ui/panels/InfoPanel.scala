package reshapes.ui.panels
import reshapes.figures.Shape
import scala.swing._
import java.awt.Color
import reshapes.Events
import scala.events.behaviour.Signal
import reshapes.Reshapes
import reshapes.EditingMode
import scala.events.scalareact

/**
 * Small info panel which displays infos like how many shapes are drawn or which shape is currently selected.
 */
class InfoPanel() extends FlowPanel {

  val currentShapeLabel = new Label { text = " " }
  val numberElementsLabel = new Label { text = "#elements: 0" }

  val currentShapeSignalNextShape = Signal {
    updateCurrentShapeLabel(Reshapes.CurrentEvents().nextShape())
  }
  val currentShapeSignalSelectedShape = Signal {
    updateCurrentShapeLabel(Reshapes.CurrentEvents().selectedShape())
  }

  contents += currentShapeLabel
  contents += new Label { text = "|" }
  contents += numberElementsLabel

  def updateCurrentShapeLabel(shape: Shape) = {
    currentShapeLabel.text = if (shape != null) shape.toString() else "";
  }

  def updateNumberElementsLabel(shapes: List[Shape]) = {
    numberElementsLabel.text = "#elements: %d".format(shapes.size)
  }
}