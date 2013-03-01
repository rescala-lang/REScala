package reshapes.panels
import scala.swing._
import reshapes.Events
import scala.swing.event._
import reshapes.Selection
import javax.swing.JColorChooser

/**
 * Panel for various customization of the stroke.
 */
class StrokeInputPanel(var events: Events) extends FlowPanel {

  def colorChooserWindow = new Frame {
    title = "Choose color"

    val colorChooser = new Component() {
      override lazy val peer = new JColorChooser()
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += colorChooser
      contents += new Button(new Action("OK") {
        def apply() = {
          confirmColor()
        }
      })
    }

    def confirmColor() = {
      events.color() = colorChooser.peer.getColor()
      visible = false
    }
  }

  val strokeWidthInput = new TextField { text = events.strokeWidth.getValue.toString(); columns = 5 }
  val colorInput = new TextField { text = "0,0,0"; columns = 10 }
  val showColorInput = new Button(new Action("Show/Hide Colorinput") {
    def apply() = {
      colorChooserWindow.visible = !colorChooserWindow.visible
    }
  })

  contents += new Label { text = "stroke width: " }
  contents += new Slider() {
    min = 1
    max = 50
    value = min
    minorTickSpacing = 1
    paintTicks = true

    reactions += {
      case e: ValueChanged => events.strokeWidth() = value
    }
  }
  contents += showColorInput

  listenTo(strokeWidthInput)
  listenTo(colorInput)

  reactions += {
    case EditDone(`strokeWidthInput`) =>
      try {
        events.strokeWidth() = strokeWidthInput.text.toInt match {
          case i if i > 0 => i
          case _ => strokeWidthInput.text = "1"; 1
        }

        events.mode match {
          case Selection() =>
            events.selectedShape.getValue.strokeWidth = events.strokeWidth.getValue
            repaint()
          case _ =>
        }
      } catch {
        case e: NumberFormatException => strokeWidthInput.text = events.strokeWidth.getValue.toString()
      }
    case EditDone(`colorInput`) =>
      try {
        val input = colorInput.text.split(',') match {
          case empty if empty.length == 1 && empty(0).isEmpty() =>
            events.color() = new Color(0, 0, 0)
            colorInput.text = "0,0,0"
          case rgbStr if rgbStr.length == 3 =>
            val rgb = rgbStr.map(x => x.toInt)
            events.color() = new Color(rgb(0), rgb(1), rgb(2))
          case _ => throw new NumberFormatException
        }

        events.mode match {
          case Selection() =>
            events.selectedShape.getValue.color = events.color.getValue
            repaint()
          case _ =>
        }
      } catch {
        case _ => colorInput.text = "%d,%d,%d".format(events.color.getValue.getRed(), events.color.getValue.getGreen(), events.color.getValue.getBlue())
      }
  }
}