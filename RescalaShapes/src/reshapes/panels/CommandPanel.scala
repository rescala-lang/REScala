package reshapes.panels
import scala.swing._
import reshapes.Events

/**
 * The CommandPanel listens all executes commands and makes it possible to revert them.
 */
class CommandPanel(var _events: Events) extends BoxPanel(Orientation.Vertical) {

  def events = _events
  def events_=(e: Events) {
    _events.Commands.changed -= updateList
    _events = e
    _events.Commands.changed += updateList

    updateList()
  }

  val commandPanel = new BoxPanel(Orientation.Vertical)
  val scrollPane = new ScrollPane()

  contents += scrollPane

  def updateList(x: Any) = {
    commandPanel.contents.clear()
    events.Commands.getValue map (command => commandPanel.contents += new Button(Action(command.getCommandDescription()) {
      command.revert()
    }))
    scrollPane.contents = commandPanel
    repaint()
  }

  updateList() // call at start
}