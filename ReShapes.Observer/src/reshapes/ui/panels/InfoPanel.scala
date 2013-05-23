package reshapes.ui.panels

import java.awt.Color

import scala.swing.FlowPanel
import scala.swing.Label

import reshapes.ReShapes
import reshapes.drawing.DrawingSpaceState
import reshapes.figures.Shape

/**
 * Small info panel which displays information like how many shapes are drawn
 * or which shape is currently selected
 */
class InfoPanel extends FlowPanel {
  val centerLabel = new Label { text = " " }
  contents += centerLabel
  
  private var currentState: DrawingSpaceState = null
  
  private var nextShape = ""
  private var selectedShape = ""
  private var shapeCount = ""
  private var strokeWidth = ""
  private var color = ""
    
  ReShapes.registerDrawingSpaceStateObserver{ state =>
    if (currentState != null) {
      currentState.unregisterNextShapeObserver(updateNextShape)
      currentState.unregisterSelectedShapeObserver(updateSelectedShape)
      currentState.unregisterShapesObserver(updateElementCount)
      currentState.unregisterStrokeWidthObserver(updateCurrentStrokeWidth)
      currentState.unregisterColorObserver(updateCurrentColor)
    }
    
    currentState = state
    if (currentState != null){
      currentState.registerNextShapeObserver(updateNextShape)
      currentState.registerSelectedShapeObserver(updateSelectedShape)
      currentState.registerShapesObserver(updateElementCount)
      currentState.registerStrokeWidthObserver(updateCurrentStrokeWidth)
      currentState.registerColorObserver(updateCurrentColor)
    }
    
    updateNextShape(if (currentState != null) state.nextShape else null)
    updateSelectedShape(if (currentState != null) state.selectedShape else null)
    updateElementCount(if (currentState != null) state.shapes else List.empty)
    updateCurrentStrokeWidth(if (currentState != null) state.strokeWidth else 1)
    updateCurrentColor(if (currentState != null) state.color else Color.BLACK)
  }
  
  def updateNextShape(shape: Shape) {
    nextShape = if (shape != null) "next shape: %s" format shape.toString else ""
    updateCenterLabel
  }
  
  def updateSelectedShape(shape: Shape) {
    selectedShape = if (shape != null) "selected: %s" format shape.toString else ""
    updateCenterLabel
  }
  
  def updateElementCount(shapes: List[Shape]) {
    shapeCount = "#elements: %d" format shapes.size
    updateCenterLabel
  }
  
  def updateCurrentStrokeWidth(width: Int) {
    strokeWidth = "stroke width: %d" format width
    updateCenterLabel
  }
  
  def updateCurrentColor(shapeColor: Color) {
    color = "color: %d-%d-%d" format
      (shapeColor.getRed, shapeColor.getGreen, shapeColor.getBlue)
    updateCenterLabel
  }
  
  def updateCenterLabel() =
    centerLabel.text = "%s | %s | %s | %s | %s" format
      (shapeCount, color, strokeWidth, nextShape, selectedShape)
}
