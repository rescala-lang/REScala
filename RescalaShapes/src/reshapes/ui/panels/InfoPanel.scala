package reshapes.ui.panels
import reshapes.figures.Shape
import scala.swing._
import java.awt.Color
import reshapes.drawing.DrawingSpaceState
import scala.events.behaviour.Signal
import reshapes.Reshapes
import reshapes.drawing.EditingMode
import scala.events.scalareact

/**
 * Small info panel which displays infos like how many shapes are drawn or which shape is currently selected.
 */
class InfoPanel() extends FlowPanel {

  val centerLabel = new Label { text = " " }

  contents += centerLabel
}
