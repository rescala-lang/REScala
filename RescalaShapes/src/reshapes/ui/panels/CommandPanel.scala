package reshapes.ui.panels

import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.ScrollPane

/**
 * The CommandPanel lists all executed commands and makes it possible to revert them
 */
class CommandPanel extends BoxPanel(Orientation.Vertical) {
  val scrollPane = new ScrollPane
  contents += scrollPane
}