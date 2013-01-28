package reshapes.panels
import scala.swing._
import scala.swing.event.ButtonClicked
import scala.swing.event.MouseClicked
import reshapes.figures.Drawable
import reshapes.command.DeleteCommand
import reshapes.Events

/**
 * Lists all drawn shapes
 */
class ShapePanel() extends ScrollPane(new BoxPanel(Orientation.Vertical)) {

  val allShapesPanel = new BoxPanel(Orientation.Vertical)

  contents = allShapesPanel

  Events.allShapes.changed += (shapes => updateAllShapesPanel(shapes))

  def updateAllShapesPanel(shapes: List[Drawable]) = {
    allShapesPanel.contents.clear()

    shapes map (shape => allShapesPanel.contents += new ShapeView(shape))
    repaint()
  }
}

class ShapeView(shape: Drawable) extends BoxPanel(Orientation.Horizontal) {
  val selectButton = new Button
  selectButton.action = new Action(shape.toString()) {
    val assignedShape = shape
    def apply() = {
      Events.selectedShape() = assignedShape
    }
  }
  val deleteButton = new Button
  deleteButton.action = new Action("delete") {
    val assignedShape = shape
    def apply() = {
      val deleteCmd = new DeleteCommand(assignedShape)
      deleteCmd.execute()
      Events.Commands() = deleteCmd :: Events.Commands.getValue
    }
  }

  contents += selectButton
  contents += deleteButton
}