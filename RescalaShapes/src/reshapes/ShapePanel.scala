package reshapes
import scala.swing._
import scala.swing.event.ButtonClicked
import scala.swing.event.MouseClicked
import reshapes.figures.Drawable

/**
 * Lists all drawn shapes
 */
class ShapePanel(events: EventHolder) extends ScrollPane(new BoxPanel(Orientation.Vertical)) {

  val allShapesPanel = new BoxPanel(Orientation.Vertical)

  contents = allShapesPanel

  events.allShapes.changed += (shapes => updateAllShapesPanel(shapes))

  def updateAllShapesPanel(shapes: List[Drawable]) = {
    val button = new Button
    button.action = new Action(shapes.head.toString()) {
      val assignedShape = shapes.head
      def apply() = {
        events.selectedShape() = assignedShape
        println(assignedShape.toString())
      }
    }
    allShapesPanel.contents += button
  }
}