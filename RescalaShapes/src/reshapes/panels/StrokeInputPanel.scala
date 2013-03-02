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
}