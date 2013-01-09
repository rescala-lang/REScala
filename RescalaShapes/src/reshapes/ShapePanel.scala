package reshapes
import scala.swing._
import scala.swing.event.ButtonClicked
import scala.swing.event.MouseClicked
import reshapes.figures.Drawable
import reshapes.command.DeleteCommand

/**
 * Lists all drawn shapes
 */
class ShapePanel(events: EventHolder) extends ScrollPane(new BoxPanel(Orientation.Vertical)) {

  val allShapesPanel = new BoxPanel(Orientation.Vertical)

  contents = allShapesPanel

  events.allShapes.changed += (shapes => updateAllShapesPanel(shapes))

  def updateAllShapesPanel(shapes: List[Drawable]) = {
    allShapesPanel.contents.clear()

    shapes map (shape => allShapesPanel.contents += new ShapeView(shape, events))
    repaint()
  }
}

class ShapeView(shape: Drawable, events: EventHolder) extends BoxPanel(Orientation.Horizontal) {
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
      val deleteCmd = new DeleteCommand(events, assignedShape)
      deleteCmd.execute()
    }
  }

  contents += selectButton
  contents += deleteButton
}