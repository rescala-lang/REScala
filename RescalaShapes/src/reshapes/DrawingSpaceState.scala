package reshapes
import java.awt.Color
import java.io.DataInputStream
import java.io.ObjectInputStream
import java.io.PrintWriter
import java.net.InetAddress
import java.net.ServerSocket
import java.net.Socket

import scala.actors.Actor
import scala.annotation.serializable

import reshapes.command.Command
import reshapes.figures.Line
import reshapes.figures.Shape

/**
 * Represents the current state of one drawing space
 */
class DrawingSpaceState {
  // selected shape to be drawn
  private var _nextShape: Shape = new Line(this)
  // currently selected shape inside the drawing space
  private var _selectedShape: Shape = null
  // currently drawn shapes
  private var _allShapes: List[Shape] = List.empty
  // current stroke width
  private var _strokeWidth: Int = 1
  // current stroke color
  private var _color: Color = Color.BLACK
  // all executed commands
  private var _commands: List[Command] = List.empty
  // filename after saving
  private var _fileName: String = "unnamed"
  
  def nextShape = _nextShape
  def selectedShape = _selectedShape
  def allShapes = _allShapes
  def strokeWidth = _strokeWidth
  def color = _color
  def commands = _commands
  def fileName = _fileName
  def mode = if (_selectedShape != null) Selection() else Drawing()
  
  def addShape(shape: Shape) =
    if (!(_allShapes contains shape)) {
      _allShapes = shape :: _allShapes
      for (obs <- allShapesObservers)
        obs(_allShapes)
    }
  
  def removeShape(shape: Shape) {
    val size = _allShapes.size
    _allShapes = _allShapes filterNot (_ == shape)
    
    if (size != _allShapes.size) {
      if (selectedShape == shape)
        selectedShape = null
      
      for (obs <- allShapesObservers)
        obs(_allShapes)
    }
  }
  
  def clear() =
    if (_allShapes.nonEmpty) {
      _allShapes = List.empty
      for (obs <- allShapesObservers)
        obs(_allShapes)
    }
    
  def nextShape_=(shape: Shape) =
    if (_nextShape != shape) {
      _nextShape = shape
      for (obs <- nextShapeObservers)
        obs(shape)
    }
  
  def selectedShape_=(shape: Shape) =
    if (_selectedShape != shape && (_allShapes contains shape)) {
      _selectedShape = shape
      for (obs <- selectedShapeObservers)
        obs(shape)
    }
  
  def strokeWidth_=(width: Int) =
    if (_strokeWidth != width) {
      _strokeWidth = width
      for (obs <- strokeWidthObservers)
        obs(width)
    }
  
  def color_=(color: Color) =
    if (_color != color) {
      _color = color
      for (obs <- colorObservers)
        obs(color)
    }
  
  def commands_=(commands: List[Command]) {
    _commands = commands
    for (obs <- CommandsObservers)
      obs(commands)
  }
  
  def fileName_=(fileName: String) =
    if (_fileName != fileName) {
      _fileName = fileName
      for (obs <- fileNameObservers)
        obs(fileName)
    }

  private var nextShapeObservers: List[(Shape => Unit)] = Nil
  private var selectedShapeObservers: List[(Shape => Unit)] = Nil
  private var allShapesObservers: List[(List[Shape] => Unit)] = Nil
  private var strokeWidthObservers: List[(Int => Unit)] = Nil
  private var colorObservers: List[(Color => Unit)] = Nil
  private var CommandsObservers: List[(List[Command] => Unit)] = Nil
  private var fileNameObservers: List[(String => Unit)] = Nil
  
  def registerNextShapeObserver(obs: Shape => Unit) =
    nextShapeObservers = obs :: nextShapeObservers
  
  def registerSelectedShapeObserver(obs: Shape => Unit) =
    selectedShapeObservers = obs :: selectedShapeObservers
  
  def registerAllShapesObserver(obs: List[Shape] => Unit) =
    allShapesObservers = obs :: allShapesObservers
  
  def registerStrokeWidthObserver(obs: Int => Unit) =
    strokeWidthObservers = obs :: strokeWidthObservers
  
  def registerColorObserver(obs: Color => Unit) =
    colorObservers = obs :: colorObservers
  
  def registerCommandsObserver(obs: List[Command] => Unit) =
    CommandsObservers = obs :: CommandsObservers
  
  def registerFileNameObserver(obs: String => Unit) =
    fileNameObservers = obs :: fileNameObservers
  
  def unregisterNextShapeObserver(obs: Shape => Unit) =
    nextShapeObservers = nextShapeObservers.filterNot(_ == obs)
  
  def unregisterSelectedShapeObserver(obs: Shape => Unit) =
    selectedShapeObservers = selectedShapeObservers.filterNot(_ == obs)
  
  def unregisterAllShapesObserver(obs: List[Shape] => Unit) =
    allShapesObservers = allShapesObservers.filterNot(_ == obs)
  
  def unregisterStrokeWidthObserver(obs: Int => Unit) =
    strokeWidthObservers = strokeWidthObservers.filterNot(_ == obs)
  
  def unregisterColorObserver(obs: Color => Unit) =
    colorObservers = colorObservers.filterNot(_ == obs)
  
  def unregisterCommandsObserver(obs: List[Command] => Unit) =
    CommandsObservers = CommandsObservers.filterNot(_ == obs)
  
  def unregisterFileNameObserver(obs: String => Unit) =
    fileNameObservers = fileNameObservers.filterNot(_ == obs)
}

/*
class NetworkSpaceState(val serverHostname: String = "localhost", val commandPort: Int = 9998, val exchangePort: Int = 9999, val listenerPort: Int = 1337) extends DrawingSpaceState {

  val serverInetAddress: InetAddress = InetAddress.getByName(serverHostname)

  /**
   * Registers this client with a server and tells him
   * which port the server has to send updates to
   */
  def registerClient(serverHostname: String, serverPort: Int, portToRegister: Int) = {
    val socket = new Socket(serverInetAddress, serverPort)
    val out = new PrintWriter(socket.getOutputStream(), true)

    out.println("register %d".format(portToRegister))

    out.close()
    socket.close()
  }

  /**
   * Starts a thread which listens to server updates.
   */
  def startUpdateListener(port: Int) = {
    new UpdateListener(port, this).start()
  }

  registerClient(serverHostname, commandPort, listenerPort)
  startUpdateListener(listenerPort)
}
*/

/**
 * Listens for updates from server and updates events.allShapes
 */
/*
class UpdateListener(port: Int, events: DrawingSpaceState) extends Actor {
  def act() {
    println("start UpdateThread")
    val listener = new ServerSocket(port)
    while (true) {
      println("receiving update")
      val socket = listener.accept()
      val in = new ObjectInputStream(new DataInputStream(socket.getInputStream()));

      val shapes = in.readObject().asInstanceOf[List[Shape]]
      events.allShapes() = List[Shape]()
      shapes map (shape => events.allShapes() = shape :: events.allShapes.getValue)

      in.close()
      socket.close()
    }
    listener.close()
  }
}
*/

abstract class EditingMode
case class Drawing() extends EditingMode
case class Selection() extends EditingMode