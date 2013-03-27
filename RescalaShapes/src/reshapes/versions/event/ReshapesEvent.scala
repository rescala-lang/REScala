package reshapes.versions.event
import java.io.DataOutputStream
import java.io.ObjectOutputStream
import java.net.Socket
import scala.annotation.serializable
import reshapes.network.TransportObject
import reshapes.ui.panels._
import reshapes.Drawing
import reshapes.DrawingSpaceState
import reshapes.NetworkSpaceState
import reshapes.Reshapes
import reshapes.Selection
import scala.events.ImperativeEvent
import reshapes.command.Command
import scala.events.Event
import reshapes.figures.Shape
import java.awt.Color

trait DrawingSpaceStateInteraction extends DrawingSpaceState {

  nextShape.changed += (shape => {
    shape.strokeWidth = strokeWidth.getValue
    shape.color = color.getValue
    allShapes.getValue map (x => x.selected = false)
    mode = Drawing()
  })

  selectedShape.changed += (shape => {
    allShapes.getValue map (x => x.selected = false)
    if (shape != null) {
      shape.selected = true
      mode = Selection()
    } else {
      mode = Drawing()
    }
  })

  strokeWidth.changed += (width => {
    if (selectedShape.getValue != null) {
      selectedShape.getValue.strokeWidth = width
    }
  })

  color.changed += (newColor => {
    if (selectedShape.getValue != null) {
      selectedShape.getValue.color = newColor
    }
  })
}

trait NetworkSpaceStateInteraction extends NetworkSpaceState {

  Commands.changed += { _ =>
    val socket = new Socket(serverInetAddress, exchangePort)
    val out = new ObjectOutputStream(new DataOutputStream(socket.getOutputStream()))

    out.writeObject(new TransportObject(allShapes.getValue, listenerPort))

    out.close()
    socket.close()
  }
}

trait CommandPanelInteraction extends CommandPanel {
  var currentState: DrawingSpaceState = null

  Reshapes.CurrentEvents.changed += { state =>
    if (currentState != null) currentState.Commands.changed -= updateList
    currentState = state
    currentState.Commands.changed += updateList
  }
}

trait InfoPanelInteraction extends InfoPanel {
  var currentState: DrawingSpaceState = null

  var nextShape = ""
  var selectedShape = ""
  var numberElements = ""
  var currentStrokeWidth = ""
  var currentColor = ""

  Reshapes.CurrentEvents.changed += { state =>
    if (currentState != null) {
      currentState.nextShape.changed -= updateNextShape
      currentState.selectedShape.changed -= updateSelectedShape
      currentState.allShapes.changed -= updateNumberElements
      currentState.strokeWidth.changed -= updateCurrentStrokeWidth
      currentState.color.changed -= updateCurrentColor
    }
    currentState = state
    currentState.nextShape.changed += updateNextShape
    currentState.selectedShape.changed += updateSelectedShape
    currentState.allShapes.changed += updateNumberElements
    currentState.strokeWidth.changed += updateCurrentStrokeWidth
    currentState.color.changed += updateCurrentColor
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

  Reshapes.CurrentEvents.changed += { state =>
    if (currentState != null) {
      currentState.allShapes.changed -= updateAllShapesPanel
    }
    currentState = state
    currentState.allShapes.changed += updateAllShapesPanel
  }
}