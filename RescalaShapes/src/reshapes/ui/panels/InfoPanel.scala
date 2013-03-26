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

  val centerLabel = new Label { text = " " }

  val nextShape: Signal[String] = Signal {
    Reshapes.CurrentEvents().nextShape()
    if (Reshapes.CurrentEvents().nextShape() != null) {
      "next shape: %s".format(Reshapes.CurrentEvents().nextShape().toString())
    } else {
      ""
    }
  }

  val selectedShape: Signal[String] = Signal {
    Reshapes.CurrentEvents().selectedShape()
    if (Reshapes.CurrentEvents().selectedShape() != null) {
      "selected: %s".format(Reshapes.CurrentEvents().selectedShape().toString())
    } else {
      "selected: [none]"
    }
  }

  val numberElements: Signal[String] = Signal {
    "#elements: %d".format(Reshapes.CurrentEvents().allShapes().size)
  }

  val currentStrokeWidth: Signal[String] = Signal {
    "stroke width: %d".format(Reshapes.CurrentEvents().strokeWidth())
  }

  val currentColor: Signal[String] = Signal {
    val color = Reshapes.CurrentEvents().color()
    "color: %s-%s-%s".format(color.getRed(), color.getGreen(), color.getBlue())
  }

  val infoText: Signal[String] = Signal {
    "%s | %s | %s | %s | %s".format(numberElements(), currentColor(), currentStrokeWidth(), nextShape(), selectedShape())
  }

  //infoText.changed += (newText => centerLabel.text = newText)
  centerLabel.text = infoText.getValue

  contents += centerLabel
}
