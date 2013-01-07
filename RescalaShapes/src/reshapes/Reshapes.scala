package reshapes

import scala.swing._
import scala.events.scalareact
import scala.events.behaviour.Var
import util.Random
import scala.swing.event._
import reshapes.figures._
import events.ImperativeEvent
import scala.events.behaviour.Signal

object Reshapes extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "ReShapes";
    preferredSize = new Dimension(1000, 500)

    val events = new EventHolder

    // GUI Elements and Layout
    val lineBtn = new Button { text = "Line" }
    val rectBtn = new Button { text = "Rectangle" }
    val ovalBtn = new Button { text = "Oval" }
    val strokeWidthInput = new TextField { text = events.strokeWidth.getValue.toString(); columns = 5 }

    contents = new BorderPanel {
      add(new FlowPanel {
        contents += new Label { text = "stroke width: " }
        contents += strokeWidthInput
      }, BorderPanel.Position.North)

      add(new BoxPanel(Orientation.Vertical) {
        contents += lineBtn
        contents += rectBtn
        contents += ovalBtn
      }, BorderPanel.Position.West)

      add(new DrawingPanel(events), BorderPanel.Position.Center)
      add(new InfoPanel(events), BorderPanel.Position.South)
    }

    // reactions
    listenTo(lineBtn)
    listenTo(rectBtn)
    listenTo(ovalBtn)
    listenTo(strokeWidthInput)

    reactions += {
      case ButtonClicked(`lineBtn`) =>
        events.selectedShape() = new Line
      case ButtonClicked(`rectBtn`) =>
        events.selectedShape() = new figures.Rectangle
      case ButtonClicked(`ovalBtn`) =>
        events.selectedShape() = new Oval
      case EditDone(`strokeWidthInput`) =>
        try {
          events.strokeWidth() = strokeWidthInput.text.toInt match {
            case i if i > 0 => i
            case _ => strokeWidthInput.text = "1"; 1
          }
        } catch {
          case e: NumberFormatException => strokeWidthInput.text = events.strokeWidth.getValue.toString()
        }
    }

    // default selected shape
    events.selectedShape() = new Line
  }
}