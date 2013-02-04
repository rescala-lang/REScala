package reshapes.panels
import scala.swing._
import scala.swing.event.ButtonClicked
import scala.swing.event.MouseClicked
import reshapes.figures.Drawable
import reshapes.command.DeleteShape
import reshapes.Events

/**
 * Lists all drawn shapes
 */
class ShapePanel(var _events: Events) extends ScrollPane(new BoxPanel(Orientation.Vertical)) {

  def events = _events
  def events_=(e: Events) {
    _events.allShapes.changed -= updateAllShapesPanel
    _events = e
    e.allShapes.changed += updateAllShapesPanel
    updateAllShapesPanel(e.allShapes.getValue)
  }

  val allShapesPanel = new BoxPanel(Orientation.Vertical)

  contents = allShapesPanel

  events.allShapes.changed += updateAllShapesPanel

  def updateAllShapesPanel(shapes: List[Drawable]) = {
    allShapesPanel.contents.clear()

    shapes map (shape => allShapesPanel.contents += new ShapeView(shape, events))
    repaint()
  }
}

class ShapeView(shape: Drawable, events: Events) extends BoxPanel(Orientation.Horizontal) {
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

  contents += selectButton
  contents += deleteButton
}