package reshapes.ui.panels
import scala.annotation.serializable
import scala.events.behaviour.Signal
import scala.swing.event.MouseClicked
import scala.swing.Color
import scala.swing.Action
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.ScrollPane

import reshapes.command.DeleteShape
import reshapes.figures.Shape
import reshapes.DrawingSpaceState
import reshapes.Reshapes

/**
 * Lists all drawn shapes
 */
class ShapePanel extends BoxPanel(Orientation.Vertical) {

  val allShapesPanel = new BoxPanel(Orientation.Vertical)

  contents += new ScrollPane(allShapesPanel)

  def updateAllShapesPanel(shapes: List[Shape]) = {
    allShapesPanel.contents.clear()

    shapes map (shape => allShapesPanel.contents += new ShapeView(shape, Reshapes.CurrentEvents.getValue))

    this.peer.revalidate()
  }
}

class ShapeView(shape: Shape, events: DrawingSpaceState) extends BoxPanel(Orientation.Horizontal) {
  val SELECTED_COLOR = new Color(0, 153, 255)
  val NOT_SELECTED_COLOR = new Color(255, 255, 255)

  val selectButton = new Button
  selectButton.action = new Action(shape.toString()) {
    val assignedShape = shape
    def apply() = {
      events.selectedShape() = assignedShape
    }
  }
  val deleteButton = new Button
  deleteButton.action = new Action("delete") {
    val assignedShape = shape
    def apply() = {
      val deleteCmd = new DeleteShape(assignedShape)
      deleteCmd.execute()
    }
  }

  contents += new Label(shape.toString())
  contents += deleteButton

  this.background = NOT_SELECTED_COLOR

  listenTo(Mouse.clicks)

  reactions += {
    case e: MouseClicked =>
      if (events.selectedShape.getValue != shape) events.selectedShape() = shape
      else {
        events.selectedShape() = null
      }
  }

  events.selectedShape.changed += (s => toggleSelection(s == shape))

  def toggleSelection(selected: Boolean) = {
    this.background = if (selected) SELECTED_COLOR else NOT_SELECTED_COLOR
  }
}