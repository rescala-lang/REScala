package reshapes.ui.panels

import java.awt.Color

import scala.swing.FlowPanel
import scala.swing.Label

import reshapes.Reshapes
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
  private var numberElements = ""
  private var currentStrokeWidth = ""
  private var currentColor = ""
    
  Reshapes.registerDrawingSpaceStateObserver{ state =>
    if (currentState != null) {
      currentState.unregisterNextShapeObserver(updateNextShape)
      currentState.unregisterSelectedShapeObserver(updateSelectedShape)
      currentState.unregisterShapesObserver(updateNumberElements)
      currentState.unregisterStrokeWidthObserver(updateCurrentStrokeWidth)
      currentState.unregisterColorObserver(updateCurrentColor)
    }
    
    currentState = state
    if (currentState != null){
      currentState.registerNextShapeObserver(updateNextShape)
      currentState.registerSelectedShapeObserver(updateSelectedShape)
      currentState.registerShapesObserver(updateNumberElements)
      currentState.registerStrokeWidthObserver(updateCurrentStrokeWidth)
      currentState.registerColorObserver(updateCurrentColor)
    }
    
    updateNextShape(if (currentState != null) state.nextShape else null)
    updateSelectedShape(if (currentState != null) state.selectedShape else null)
    updateNumberElements(if (currentState != null) state.shapes else List.empty)
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
  
  def updateNumberElements(shapes: List[Shape]) {
    numberElements = "#elements: %d" format shapes.size
    updateCenterLabel
  }
  
  def updateCurrentStrokeWidth(width: Int) {
    currentStrokeWidth = "stroke width: %d" format width
    updateCenterLabel
  }
  
  def updateCurrentColor(color: Color) {
    currentColor = "color: %d-%d-%d" format (color.getRed(), color.getGreen(), color.getBlue())
    updateCenterLabel
  }
  
  def updateCenterLabel() =
    centerLabel.text = "%s | %s | %s | %s | %s" format
      (numberElements, currentColor, currentStrokeWidth, nextShape, selectedShape)
}
