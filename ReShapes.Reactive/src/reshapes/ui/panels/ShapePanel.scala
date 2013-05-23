package reshapes.ui.panels

import scala.events.Event
import scala.events.behaviour.Signal
import scala.swing.BoxPanel
import scala.swing.Color
import scala.swing.Component
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.ScrollPane

import reshapes.ReShapes
import reshapes.drawing.DeleteShape
import reshapes.drawing.DrawingSpaceState
import reshapes.figures.Shape
import reshapes.util.ReactiveUtil.UnionEvent
import reswing.ImperativeSignal.fromValue
import reswing.ReBoxPanel
import reswing.ReButton
import reswing.ReButton.toButton

/**
 * Lists all drawn shapes
 */
class ShapePanel extends BoxPanel(Orientation.Vertical) {
  def state = ReShapes.drawingSpaceState
  
  val shapes = Signal { if (state() != null) state().shapes() else List.empty }
  
  val shapeViews = Signal { shapes() map { shape => new ShapeView(shape, state()) } }
  
  val shapesPanel = ReBoxPanel(
    orientation = Orientation.Vertical,
    contents = Signal[Seq[Component]] {
      shapeViews() map { shapeView: ShapeView => shapeView: Component } })
  
  contents += new ScrollPane {
    contents = shapesPanel
  }
  
  val deleted = UnionEvent(Signal { shapeViews() map { shapeView => shapeView.deleted } })
}

class ShapeView(shape: Shape, state: DrawingSpaceState) extends ReBoxPanel(Orientation.Horizontal) {
  val SELECTED_COLOR = new Color(0, 153, 255)
  val NOT_SELECTED_COLOR = new Color(255, 255, 255)
  
  val deleteButton = ReButton("delete")
  
  val deleted: Event[DeleteShape] =
    deleteButton.clicked map { _: Any => new DeleteShape(shape) }
  
  peer.background = NOT_SELECTED_COLOR
  peer.contents += new Label(shape.toString)
  peer.contents += deleteButton
  
  mouse.clicks.clicked += { _ =>
    state.select(if (state.selectedShape.getValue != shape) shape else null) }
  
  state.selectedShape.changed += { selected =>
    peer.background = if (selected == shape) SELECTED_COLOR else NOT_SELECTED_COLOR
  }
}