package reshapes.ui.panels
import scala.swing._
import reshapes.DrawingSpaceState
import scala.events.behaviour.Signal
import reshapes.Reshapes
import reshapes.command.Command

/**
 * The CommandPanel listens all executes commands and makes it possible to revert them.
 */
class CommandPanel extends BoxPanel(Orientation.Vertical) {

  val scrollPane = new ScrollPane()

  contents += scrollPane
}