package reshapes.panels
import scala.swing._
import reshapes.Events

class CommandPanel extends BoxPanel(Orientation.Vertical) {
  val commandPanel = new BoxPanel(Orientation.Vertical)
  val scrollPane = new ScrollPane()

  contents += scrollPane

  def updateList() = {
    commandPanel.contents.clear()
    Events.Commands.getValue map (command => commandPanel.contents += new Button(Action(command.getCommandDescription()) {
      command.revert()
    }))
    scrollPane.contents = commandPanel
    repaint()
  }

  Events.Commands.changed += (_ => updateList())
  updateList() // call at start
}