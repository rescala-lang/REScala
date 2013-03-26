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

  val commandPanel = new BoxPanel(Orientation.Vertical)
  val scrollPane = new ScrollPane()

  val updateSignal: Signal[List[Command]] = Signal {
    Reshapes.CurrentEvents().Commands()
  }

  updateSignal.changed += updateList

  contents += scrollPane

  def updateList(commands: List[Command]) = {
    commandPanel.contents.clear()
    commands.map(command => commandPanel.contents += new Button(Action(command.getCommandDescription()) {
      command.revert()
    }))
    scrollPane.contents = commandPanel
    repaint()
  }
}