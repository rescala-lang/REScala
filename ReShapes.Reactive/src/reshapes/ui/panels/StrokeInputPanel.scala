package reshapes.ui.panels

import java.awt.Color

import scala.events.behaviour.Var
import scala.swing.Action
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Component
import scala.swing.FlowPanel
import scala.swing.Frame
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.Slider
import scala.swing.event.ValueChanged

import javax.swing.JColorChooser

/**
 * Panel for various customization of the stroke.
 */
class StrokeInputPanel extends FlowPanel {
  def colorChooserWindow = new Frame {
    title = "Choose color"
    
    val colorChooser = new Component {
      override lazy val peer = new JColorChooser
    }
    
    contents = new BoxPanel(Orientation.Vertical) {
      contents += colorChooser
      contents += new Button(Action("OK") { confirmColor })
    }
    
    def confirmColor() {
      StrokeInputPanel.this.color() = colorChooser.peer.getColor
      visible = false
    }
  }
  
  val showColorInput = new Button(new Action("Show Colorinput") {
    def apply() = {
      colorChooserWindow.visible = !colorChooserWindow.visible
    }
  })
  
  val strokeWidth = Var(1)
  val color = Var(Color.BLACK)
  
  contents += new Label { text = "stroke width: " }
  contents += new Slider {
    min = 1
    max = 50
    value = min
    minorTickSpacing = 1
    paintTicks = true
    
    reactions += {
      case e: ValueChanged => StrokeInputPanel.this.strokeWidth() = value
    }
  }
  contents += showColorInput
}