package reshapes.ui.panels
import scala.swing._
import reshapes.DrawingSpaceState
import scala.swing.event._
import reshapes.Selection
import javax.swing.JColorChooser
import reshapes.Reshapes

/**
 * Panel for various customization of the stroke.
 */
class StrokeInputPanel() extends FlowPanel {

  def colorChooserWindow = new Frame {
    title = "Choose color"

    // the ColorChooser wasn't available in the standard scala library so I had to use this construct.
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
      Reshapes.CurrentEvents.getValue.color() = colorChooser.peer.getColor()
      visible = false
    }
  }

  val showColorInput = new Button(new Action("Show Colorinput") {
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
      case e: ValueChanged => Reshapes.CurrentEvents.getValue.strokeWidth() = value
    }
  }
  contents += showColorInput
}