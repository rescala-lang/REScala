package reshapes.ui.panels

import scala.swing.Action
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Color
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.ScrollPane
import scala.swing.event.MouseClicked

import reshapes.ReShapes
import reshapes.drawing.DeleteShape
import reshapes.drawing.DrawingSpaceState
import reshapes.figures.Shape

/**
 * Lists all drawn shapes
 */
class ShapePanel extends BoxPanel(Orientation.Vertical) {
  val shapesPanel = new BoxPanel(Orientation.Vertical)
  contents += new ScrollPane {
    contents = shapesPanel
  }
  
  var currentState: DrawingSpaceState = null
  
  ReShapes.drawingSpaceStateChanged += { state => //#HDL
    if (currentState != null)
      currentState.shapesChanged -= updateShapesPanel
    
    currentState = state
    if (currentState != null)
      currentState.shapesChanged += updateShapesPanel //#HDL
    
    updateShapesPanel(if (currentState != null) state.shapes else List.empty)
  }
  
  def updateShapesPanel(shapes: List[Shape]) {
    shapesPanel.contents.clear
    for (shape <- shapes)
      shapesPanel.contents += new ShapeView(shape, currentState)
    repaint
  }
}

class ShapeView(shape: Shape, state: DrawingSpaceState) extends BoxPanel(Orientation.Horizontal) {
  val SELECTED_COLOR = new Color(0, 153, 255)
  val NOT_SELECTED_COLOR = new Color(255, 255, 255)
  
  val deleteButton = new Button(Action("delete") {
    state execute new DeleteShape(shape)
  })
  
  contents += new Label(shape.toString)
  contents += deleteButton
  
  background = NOT_SELECTED_COLOR
  
  listenTo(mouse.clicks)
  
  reactions += {  //#HDL
    case e: MouseClicked =>
      state.selectedShape = if (state.selectedShape != shape) shape else null
  }
  
  state.selectedShapeChanged += { selected => //#HDL
    background = if (selected == shape) SELECTED_COLOR else NOT_SELECTED_COLOR
  }
}