package reshapes.versions.observer

import java.awt.Color
import java.io.OutputStreamWriter
import java.net.Socket

import scala.swing.Action
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Orientation
import scala.xml.XML

import reshapes.Reshapes
import reshapes.drawing.Command
import reshapes.drawing.DrawingSpaceState
import reshapes.drawing.NetworkSpaceState
import reshapes.figures.Shape
import reshapes.ui.panels.CommandPanel
import reshapes.ui.panels.DrawingPanel
import reshapes.ui.panels.InfoPanel
import reshapes.ui.panels.ShapePanel
import reshapes.ui.panels.ShapeView

trait CommandPanelInteraction extends CommandPanel {
  var currentState: DrawingSpaceState = null
  val commandPanel = new BoxPanel(Orientation.Vertical)
  
  Reshapes.registerDrawingSpaceStateObserver{ state =>
    if (currentState != null)
      currentState.unregisterCommandsObserver(updateList)
    
    currentState = state
    if (currentState != null)
      currentState.registerCommandsObserver(updateList)
    
    updateList(if (currentState != null) state.commands else List.empty)
  }
  
  def updateList(commands: List[Command]) {
    commandPanel.contents.clear
    for (command <- commands)
      commandPanel.contents +=
        new Button(Action(command.getCommandDescription())
            { currentState revert command })
    repaint
  }
  
  scrollPane.contents = commandPanel
}

trait InfoPanelInteraction extends InfoPanel {
  var currentState: DrawingSpaceState = null
  
  var nextShape = ""
  var selectedShape = ""
  var numberElements = ""
  var currentStrokeWidth = ""
  var currentColor = ""
    
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

trait ShapePanelInteraction extends ShapePanel {
  var currentState: DrawingSpaceState = null
  val shapesPanel = new BoxPanel(Orientation.Vertical)
  
  Reshapes.registerDrawingSpaceStateObserver{ state =>
    if (currentState != null)
      currentState.unregisterShapesObserver(updateShapesPanel)
    
    currentState = state
    if (currentState != null)
      currentState.registerShapesObserver(updateShapesPanel)
    
    updateShapesPanel(if (currentState != null) state.shapes else List.empty)
  }
  
  def updateShapesPanel(shapes: List[Shape]) {
    shapesPanel.contents.clear
    for (shape <- shapes)
      shapesPanel.contents += new ShapeView(shape, currentState)
    repaint
  }
  
  scrollPane.contents = shapesPanel
}

trait DrawingPanelInteraction extends DrawingPanel {
  state.registerSelectedShapeObserver(canvasChange)
  state.registerShapesObserver(canvasChange)
  state.registerStrokeWidthObserver(canvasChange)
  state.registerColorObserver(canvasChange)
  
  def canvasChange(x: Any) = repaint
}
