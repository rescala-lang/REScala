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

trait NetworkSpaceStateInteraction extends NetworkSpaceState {

  registerAllShapesObserver{ shapes =>
    if (!updating) {
      val socket = new Socket(serverInetAddress, exchangePort)
      val writer = new OutputStreamWriter(socket.getOutputStream)
      XML.write(writer, Shape.serialize(shapes), "", false, null)
      writer.close
      socket.close
    }
  }
}

trait CommandPanelInteraction extends CommandPanel {
  var currentState: DrawingSpaceState = null
  val commandPanel = new BoxPanel(Orientation.Vertical)

  Reshapes.registerCurrentEventsObserver{ state =>
    state.registerCommandsObserver(updateList)
    updateList(state.commands)
  }

  def updateList(commands: List[Command]) = {
    commandPanel.contents.clear()
    commands.map(command => commandPanel.contents += new Button(Action(command.getCommandDescription()) {
      Reshapes.currentEvents revert command
    }))
    scrollPane.contents = commandPanel
    repaint()
  }
}

trait InfoPanelInteraction extends InfoPanel {
  var currentState: DrawingSpaceState = null

  var nextShape = ""
  var selectedShape = ""
  var numberElements = ""
  var currentStrokeWidth = ""
  var currentColor = ""

  Reshapes.registerCurrentEventsObserver{ state =>
    state.registerNextShapeObserver(updateNextShape)
    state.registerSelectedShapeObserver(updateSelectedShape)
    state.registerAllShapesObserver(updateNumberElements)
    state.registerStrokeWidthObserver(updateCurrentStrokeWidth)
    state.registerColorObserver(updateCurrentColor)
    
    updateNextShape(state.nextShape)
    updateSelectedShape(state.selectedShape)
    updateNumberElements(state.allShapes)
    updateCurrentStrokeWidth(state.strokeWidth)
    updateCurrentColor(state.color)
  }

  def updateNextShape(shape: Shape) = {
    if (shape != null) nextShape = "next shape: %s".format(shape.toString())
    else nextShape = ""
    updateCenterLabel()
  }

  def updateSelectedShape(shape: Shape) = {
    if (shape != null) selectedShape = "selected: %s".format(shape.toString())
    else selectedShape = ""
    updateCenterLabel()
  }

  def updateNumberElements(shapes: List[Shape]) = {
    numberElements = "#elements: %d".format(shapes.size)
    updateCenterLabel()
  }

  def updateCurrentStrokeWidth(width: Int) = {
    currentStrokeWidth = "stroke width: %d".format(width)
    updateCenterLabel()
  }

  def updateCurrentColor(color: Color) = {
    currentColor = "color: %d-%d-%d".format(color.getRed(), color.getGreen(), color.getBlue())
    updateCenterLabel()
  }

  def updateCenterLabel() = {
    centerLabel.text = "%s | %s | %s | %s | %s".format(numberElements, currentColor, currentStrokeWidth, nextShape, selectedShape)
  }
}

trait ShapePanelInteraction extends ShapePanel {
  var currentState: DrawingSpaceState = null
  val allShapesPanel = new BoxPanel(Orientation.Vertical)

  Reshapes.registerCurrentEventsObserver{ state =>
    state.registerAllShapesObserver(updateAllShapesPanel)
    updateAllShapesPanel(state.allShapes)
  }

  def updateAllShapesPanel(shapes: List[Shape]) = {
    allShapesPanel.contents.clear()

    shapes map (shape => allShapesPanel.contents += new ShapeView(shape, Reshapes.currentEvents))

    scrollPane.contents = allShapesPanel

    this.peer.revalidate()
  }
}

trait DrawingPanelInteraction extends DrawingPanel {

  val state = event
  state.registerSelectedShapeObserver(canvasChange)
  state.registerAllShapesObserver(canvasChange)
  state.registerStrokeWidthObserver(canvasChange)
  state.registerColorObserver(canvasChange)

  def canvasChange(x: Any) = {
    repaint()
  }
}
