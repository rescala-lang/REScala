/**
 * Because of the architecture of the application this file does not contain
 * a completly correct usage of observer pattern. It simply shows the difference
 * to event/signals 'in principal': Instead of each (gui-)panel defining its own
 * events/signals they have to DrawingSpaceStateInteraction as Observers and
 * DrawingSpaceStateInteraction explicitly calls the observer methods.
 */
package reshapes.versions.observer

import java.io.DataOutputStream
import java.io.ObjectOutputStream
import java.net.Socket
import scala.annotation.serializable
import reshapes.network.TransportObject
import reshapes.ui.panels._
import reshapes.drawing.Drawing
import reshapes.drawing.DrawingSpaceState
import reshapes.Reshapes
import reshapes.drawing.Selection
import reshapes.drawing.Command
import reshapes.figures.Shape
import java.awt.Color
import reshapes.drawing.EditingMode
import scala.swing.Button
import scala.collection.mutable.MutableList
import scala.swing.Action
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Panel

/*
trait NetworkSpaceStateInteraction extends NetworkSpaceState {

  val sendUpdateSignal: Signal[List[Shape]] = Signal {
    Commands()
    allShapes.getValue
  }

  sendUpdateSignal.changed += { shapes =>
    val socket = new Socket(serverInetAddress, exchangePort)
    val out = new ObjectOutputStream(new DataOutputStream(socket.getOutputStream()))

    out.writeObject(new TransportObject(shapes, listenerPort))

    out.close()
    socket.close()
  }
}
*/
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
