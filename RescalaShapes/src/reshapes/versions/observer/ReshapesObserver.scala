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
import scala.events.behaviour.Signal
import reshapes.EditingMode
import scala.swing.Button
import scala.collection.mutable.MutableList
import scala.swing.Action
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Panel

/**
 * Change events in this class are used as setters.
 * The 'clean' way would be to reimplement DrawingSpaceState without
 * the usage of Var[] and just use the standard datatypes with scalas
 * setter/getter methods
 */
trait DrawingSpaceStateInteraction extends DrawingSpaceState {

  nextShape.changed += { shape =>
    for (obs <- nextShapeObservers) obs(shape)
    mode = Drawing()
  }
  selectedShape.changed += { shape =>
    for (obs <- selectedShapeObservers) obs(shape)
    if (shape != null) {
      mode = Selection()
      shape.selected = true
    } else mode = Drawing()
  }
  allShapes.changed += { shapes => for (obs <- allShapesObservers) obs(shapes) }
  strokeWidth.changed += { width => for (obs <- strokeWidthObservers) obs(width) }
  color.changed += { newColor => for (obs <- colorObservers) obs(newColor) }
  Commands.changed += { commands => for (obs <- CommandsObservers) obs(commands) }
  fileName.changed += { name => for (obs <- fileNameObservers) obs(name) }

  var nextShapeObservers: List[(Shape => Unit)] = Nil
  var selectedShapeObservers: List[(Shape => Unit)] = Nil
  var allShapesObservers: List[(List[Shape] => Unit)] = Nil
  var strokeWidthObservers: List[(Int => Unit)] = Nil
  var colorObservers: List[(Color => Unit)] = Nil
  var CommandsObservers: List[(List[Command] => Unit)] = Nil
  var fileNameObservers: List[(String => Unit)] = Nil

  def registerNextShapeObserver(obs: (Shape => Unit)) = {
    nextShapeObservers = obs :: nextShapeObservers
  }

  def registerSelectedShapeObserver(obs: (Shape => Unit)) = {
    selectedShapeObservers = obs :: selectedShapeObservers
  }

  def registerAllShapesObserver(obs: (List[Shape] => Unit)) = {
    allShapesObservers = obs :: allShapesObservers
  }

  def registerStrokeWidthObserver(obs: (Int => Unit)) = {
    strokeWidthObservers = obs :: strokeWidthObservers
  }

  def registerColorObserver(obs: (Color => Unit)) = {
    colorObservers = obs :: colorObservers
  }

  def registerCommandsObserver(obs: (List[Command] => Unit)) = {
    CommandsObservers = obs :: CommandsObservers
    println(CommandsObservers)
  }

  def registerFileNameObserver(obs: (String => Unit)) = {
    fileNameObservers = obs :: fileNameObservers
  }

  def unregisterNextShapeObserver(obs: (Shape => Unit)) = {
    nextShapeObservers = nextShapeObservers.filterNot(_ == obs)
  }

  def unregisterSelectedShapeObserver(obs: (Shape => Unit)) = {
    selectedShapeObservers = selectedShapeObservers.filterNot(_ == obs)
  }

  def unregisterAllShapesObserver(obs: (List[Shape] => Unit)) = {
    allShapesObservers = allShapesObservers.filterNot(_ == obs)
  }

  def unregisterStrokeWidthObserver(obs: (Int => Unit)) = {
    strokeWidthObservers = strokeWidthObservers.filterNot(_ == obs)
  }

  def unregisterColorObserver(obs: (Color => Unit)) = {
    colorObservers = colorObservers.filterNot(_ == obs)
  }

  def unregisterCommandsObserver(obs: (List[Command] => Unit)) = {
    CommandsObservers = CommandsObservers.filterNot(_ == obs)
  }

  def unregisterFileNameObserver(obs: (String => Unit)) = {
    fileNameObservers = fileNameObservers.filterNot(_ == obs)
  }
}

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

trait CommandPanelInteraction extends CommandPanel {
  var currentState: DrawingSpaceState = null
  val commandPanel = new BoxPanel(Orientation.Vertical)

  Reshapes.CurrentEvents.changed += { state =>
    var currentState = state.asInstanceOf[DrawingSpaceStateInteraction].registerCommandsObserver(updateList)
  }

  def updateList(commands: List[Command]) = {
    commandPanel.contents.clear()
    commands.map(command => commandPanel.contents += new Button(Action(command.getCommandDescription()) {
      command.revert()
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

  Reshapes.CurrentEvents.changed += { state =>
    val currentState = state.asInstanceOf[DrawingSpaceStateInteraction]
    currentState.registerNextShapeObserver(updateNextShape)
    currentState.registerSelectedShapeObserver(updateSelectedShape)
    currentState.registerAllShapesObserver(updateNumberElements)
    currentState.registerStrokeWidthObserver(updateCurrentStrokeWidth)
    currentState.registerColorObserver(updateCurrentColor)
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

  Reshapes.CurrentEvents.changed += { state =>
    state.asInstanceOf[DrawingSpaceStateInteraction].registerAllShapesObserver(updateAllShapesPanel)
  }

  def updateAllShapesPanel(shapes: List[Shape]) = {
    allShapesPanel.contents.clear()

    shapes map (shape => allShapesPanel.contents += new ShapeView(shape, Reshapes.CurrentEvents.getValue))

    scrollPane.contents = allShapesPanel

    this.peer.revalidate()
  }
}

trait DrawingPanelInteraction extends DrawingPanel {

  val state = event.asInstanceOf[DrawingSpaceStateInteraction]
  state.registerSelectedShapeObserver(canvasChange)
  state.registerAllShapesObserver(canvasChange)
  state.registerStrokeWidthObserver(canvasChange)
  state.registerColorObserver(canvasChange)

  def canvasChange(x: Any) = {
    repaint()
  }
}
