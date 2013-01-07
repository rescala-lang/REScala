package reshapes
import scala.events.ImperativeEvent
import reshapes.figures.Drawable
import scala.swing._

class InfoPanel(events: EventHolder) extends FlowPanel {

  val currentShapeLabel = new Label { text = " " }

  events.selectedShape.changed += (selectedShape => currentShapeLabel.text = selectedShape.toString())

  contents += currentShapeLabel
}