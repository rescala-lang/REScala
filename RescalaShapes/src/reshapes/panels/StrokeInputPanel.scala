package reshapes.panels
import scala.swing._
import reshapes.Events
import scala.swing.event._
import reshapes.Selection

/**
 * Panel for various customization of the stroke.
 */
class StrokeInputPanel extends FlowPanel {

  val strokeWidthInput = new TextField { text = Events.strokeWidth.getValue.toString(); columns = 5 }
  val colorInput = new TextField { text = "0,0,0"; columns = 10 }
  val shapePanel = new ShapePanel()

  contents += new Label { text = "stroke width: " }
  contents += strokeWidthInput
  contents += new Label { text = "stroke color: " }
  contents += colorInput

  listenTo(strokeWidthInput)
  listenTo(colorInput)

  reactions += {
    case EditDone(`strokeWidthInput`) =>
      try {
        Events.strokeWidth() = strokeWidthInput.text.toInt match {
          case i if i > 0 => i
          case _ => strokeWidthInput.text = "1"; 1
        }

        Events.mode match {
          case Selection() =>
            Events.selectedShape.getValue.strokeWidth = Events.strokeWidth.getValue
            repaint()
          case _ =>
        }
      } catch {
        case e: NumberFormatException => strokeWidthInput.text = Events.strokeWidth.getValue.toString()
      }
    case EditDone(`colorInput`) =>
      try {
        val input = colorInput.text.split(',') match {
          case empty if empty.length == 1 && empty(0).isEmpty() =>
            Events.color() = new Color(0, 0, 0)
            colorInput.text = "0,0,0"
          case rgbStr if rgbStr.length == 3 =>
            val rgb = rgbStr.map(x => x.toInt)
            Events.color() = new Color(rgb(0), rgb(1), rgb(2))
          case _ => throw new NumberFormatException
        }

        Events.mode match {
          case Selection() =>
            Events.selectedShape.getValue.color = Events.color.getValue
            repaint()
          case _ =>
        }
      } catch {
        case _ => colorInput.text = "%d,%d,%d".format(Events.color.getValue.getRed(), Events.color.getValue.getGreen(), Events.color.getValue.getBlue())
      }
  }
}