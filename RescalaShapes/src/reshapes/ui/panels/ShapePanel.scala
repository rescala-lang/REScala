package reshapes.ui.panels
import scala.swing._
import scala.swing.event.ButtonClicked
import scala.swing.event.MouseClicked
import reshapes.figures.Shape
import reshapes.command.DeleteShape
import reshapes.Events
import scala.swing.event.MouseClicked
import scala.events.behaviour.Var
import reshapes.figures.Line
import scala.swing.event.KeyPressed
import scala.swing.event.Key
import scala.events.behaviour.Signal
import reshapes.Reshapes

/**
 * Lists all drawn shapes
 */
class ShapePanel() extends ScrollPane(new BoxPanel(Orientation.Vertical)) {

  val allShapesPanel = new BoxPanel(Orientation.Vertical)

  contents = allShapesPanel

  val allShapesChangedSignal = Signal {
    updateAllShapesPanel(Reshapes.CurrentEvents().allShapes())
  }

  def updateAllShapesPanel(shapes: List[Shape]) = {
    allShapesPanel.contents.clear()

    shapes map (shape => allShapesPanel.contents += new ShapeView(shape, Reshapes.CurrentEvents.getValue))
    repaint()
  }
}

class ShapeView(shape: Shape, events: Events) extends BoxPanel(Orientation.Horizontal) {
  val SELECTED_COLOR = new Color(0, 153, 255)
  val NOT_SELECTED_COLOR = new Color(255, 255, 255)

  var selected = false

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
  listenTo(keys)

  reactions += {
    case e: MouseClicked => events.selectedShape() = shape
    case KeyPressed(_, Key.Space, _, _) => println("ctrl pressed")
  }

  events.selectedShape.changed += (s => toggleSelection(s == shape))

  def toggleSelection(selected: Boolean) = {
    this.background = if (selected) SELECTED_COLOR else NOT_SELECTED_COLOR
    if (selected) {
      this.background = SELECTED_COLOR
    } else {
      this.background = NOT_SELECTED_COLOR
    }
  }
}