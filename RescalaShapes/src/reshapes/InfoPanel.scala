package reshapes
import scala.events.ImperativeEvent
import reshapes.figures.Drawable
import scala.swing._

class InfoPanel(events: EventHolder) extends FlowPanel {

  val currentShapeLabel = new Label { text = " " }
  val numberElementsLabel = new Label { text = "#elements: 0" }

  events.selectedShape.changed += (selectedShape => currentShapeLabel.text = selectedShape.toString())
  events.allShapes.changed += (shapes => numberElementsLabel.text = "#elements: %d".format(shapes.size))

  contents += currentShapeLabel
  contents += numberElementsLabel
}