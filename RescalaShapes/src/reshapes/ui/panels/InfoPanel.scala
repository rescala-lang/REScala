package reshapes.ui.panels

import scala.swing.FlowPanel
import scala.swing.Label

/**
 * Small info panel which displays information like how many shapes are drawn
 * or which shape is currently selected
 */
class InfoPanel extends FlowPanel {
  val centerLabel = new Label { text = " " }
  contents += centerLabel
}
