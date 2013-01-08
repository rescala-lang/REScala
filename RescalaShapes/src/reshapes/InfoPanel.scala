package reshapes
import scala.events.ImperativeEvent
import reshapes.figures.Drawable
import scala.swing._
import java.awt.Color

class InfoPanel(events: EventHolder) extends FlowPanel {

  val currentShapeLabel = new Label { text = " " }
  val numberElementsLabel = new Label { text = "#elements: 0" }
  val modeLabel = new Label { text = "mode: " }

  events.nextShape.changed += (nextShape => currentShapeLabel.text = nextShape.toString())
  events.allShapes.changed += (shapes => numberElementsLabel.text = "#elements: %d".format(shapes.size))
  events.modeChange += (_ => modeLabel.text = "mode: %s".format(events.mode.getClass().getSimpleName()))

  contents += currentShapeLabel
  contents += new Label { text = "|" }
  contents += numberElementsLabel
  contents += new Label { text = "|" }
  contents += modeLabel
}