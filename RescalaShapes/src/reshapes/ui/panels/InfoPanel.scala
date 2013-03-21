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

  val currentlyNextShape: Signal[String] = Signal {
    Reshapes.CurrentEvents().nextShape()
    if (Reshapes.CurrentEvents().nextShape() != null) {
      "Next shape: %s".format(Reshapes.CurrentEvents().nextShape().toString())
    } else {
      ""
    }
  }

  val currentlySelectedShape: Signal[String] = Signal {
    Reshapes.CurrentEvents().selectedShape()
    if (Reshapes.CurrentEvents().selectedShape() != null) {
      Reshapes.CurrentEvents().selectedShape().toString()
    } else {
      ""
    }
  }

  val numberElements: Signal[String] = Signal {
    "#elements: %d".format(Reshapes.CurrentEvents().allShapes().size)
  }

  numberElements.changed += (newText => numberElementsLabel.text = newText)
  currentlySelectedShape.changed += (newText => currentShapeLabel.text = newText)
  currentlyNextShape.changed += (newText => currentShapeLabel.text = newText)

  contents += currentShapeLabel
  contents += new Label { text = "|" }
  contents += numberElementsLabel
}
