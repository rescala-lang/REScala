package reshapes.ui.panels

import scala.swing.Action
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Orientation
import scala.swing.ScrollPane

import reshapes.ReShapes
import reshapes.drawing.Command
import reshapes.drawing.DrawingSpaceState

/**
 * The CommandPanel lists all executed commands and makes it possible to revert them
 */
class CommandPanel extends BoxPanel(Orientation.Vertical) {
  val commandPanel = new BoxPanel(Orientation.Vertical)
  
  contents += new ScrollPane {
    contents = commandPanel
  }
  
  private var currentState: DrawingSpaceState = null
  
  ReShapes.registerDrawingSpaceStateObserver{ state =>
    if (currentState != null)
      currentState.unregisterCommandsObserver(updateList)
    
    currentState = state
    if (currentState != null)
      currentState.registerCommandsObserver(updateList)
    
    updateList(if (currentState != null) state.commands else List.empty)
  }
  
  def updateList(commands: List[Command]) {
    commandPanel.contents.clear
    for (command <- commands)
      commandPanel.contents +=
        new Button(Action(command.description)
            { currentState revert command })
    repaint
  }
}