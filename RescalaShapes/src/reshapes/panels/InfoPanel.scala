package reshapes.panels
import scala.events.ImperativeEvent
import reshapes.figures.Drawable
import scala.swing._
import java.awt.Color
import reshapes.Events

class InfoPanel() extends FlowPanel {

  val currentShapeLabel = new Label { text = " " }
  val numberElementsLabel = new Label { text = "#elements: 0" }
  val modeLabel = new Label { text = "mode: " }

  Events.nextShape.changed += (nextShape => currentShapeLabel.text = nextShape.toString())
  Events.selectedShape.changed += (selectedShape => currentShapeLabel.text = selectedShape.toString())
  Events.allShapes.changed += (shapes => numberElementsLabel.text = "#elements: %d".format(shapes.size))
  Events.modeChange += (_ => modeLabel.text = "mode: %s".format(Events.mode.getClass().getSimpleName()))

  contents += currentShapeLabel
  contents += new Label { text = "|" }
  contents += numberElementsLabel
  contents += new Label { text = "|" }
  contents += modeLabel
}