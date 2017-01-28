package reshapes.drawing

import java.awt.Color
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.net.InetAddress
import java.net.ServerSocket
import java.net.Socket
import java.net.SocketException

import scala.actors.Actor
import scala.xml.Attribute
import scala.xml.Null
import scala.xml.Text
import scala.xml.XML

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
  private var _shapes: List[Shape] = List.empty
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
  def shapes = _shapes
  def strokeWidth = _strokeWidth
  def color = _color
  def commands = _commands
  def fileName = _fileName
  
  def execute(command: Command) {
    _commands ::= command
    for (obs <- commandsObservers)
      obs(_commands)
    
    val shapes = command execute _shapes
    if (shapes != _shapes) {
      if (_selectedShape != null && !(shapes contains _selectedShape)) {
        _selectedShape = null
        for (obs <- selectedShapeObservers)
          obs(_selectedShape)
      }
      
      _shapes = shapes
      for (obs <- shapesObservers)
        obs(_shapes)
    }
  }
  
  def revert(command: Command) {
    val count = (_commands indexOf command) + 1
    if (count != 0) {
      val shapes = (_shapes /: (_commands take count)) {
        (shapes, command) => command revert shapes
      }
      
      if (shapes != _shapes) {
        if (_selectedShape != null && !(shapes contains _selectedShape)) {
          _selectedShape = null
          for (obs <- selectedShapeObservers)
           obs(_selectedShape)
        }
        
        _shapes = shapes
        for (obs <- shapesObservers)
          obs(_shapes)
      }
      
      _commands = _commands drop count
      for (obs <- commandsObservers)
        obs(_commands)
    }
  }
  
  def clear() =
    if (_shapes.nonEmpty) {
      _shapes = List.empty
      for (obs <- shapesObservers)
        obs(_shapes)
      
      _commands = List.empty
      for (obs <- commandsObservers)
        obs(_commands)
    }
    
  def nextShape_=(shape: Shape) =
    if (_nextShape != shape) {
      _nextShape = shape
      for (obs <- nextShapeObservers)
        obs(shape)
    }
  
  def selectedShape_=(shape: Shape) =
    if (_selectedShape != shape && (shape == null || (_shapes contains shape))) {
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
  
  def fileName_=(fileName: String) =
    if (_fileName != fileName) {
      _fileName = fileName
      for (obs <- fileNameObservers)
        obs(fileName)
    }

  private var nextShapeObservers: List[Shape => Unit] = Nil
  private var selectedShapeObservers: List[Shape => Unit] = Nil
  private var shapesObservers: List[List[Shape] => Unit] = Nil
  private var strokeWidthObservers: List[Int => Unit] = Nil
  private var colorObservers: List[Color => Unit] = Nil
  private var commandsObservers: List[List[Command] => Unit] = Nil
  private var fileNameObservers: List[String => Unit] = Nil
  
  def registerNextShapeObserver(obs: Shape => Unit) =
    nextShapeObservers ::= obs
  
  def registerSelectedShapeObserver(obs: Shape => Unit) =
    selectedShapeObservers ::= obs
  
  def registerShapesObserver(obs: List[Shape] => Unit) =
    shapesObservers ::= obs
  
  def registerStrokeWidthObserver(obs: Int => Unit) =
    strokeWidthObservers ::= obs
  
  def registerColorObserver(obs: Color => Unit) =
    colorObservers ::= obs
  
  def registerCommandsObserver(obs: List[Command] => Unit) =
    commandsObservers ::= obs
  
  def registerFileNameObserver(obs: String => Unit) =
    fileNameObservers ::= obs
  
  def unregisterNextShapeObserver(obs: Shape => Unit) =
    nextShapeObservers = nextShapeObservers filterNot (_ == obs)
  
  def unregisterSelectedShapeObserver(obs: Shape => Unit) =
    selectedShapeObservers = selectedShapeObservers filterNot (_ == obs)
  
  def unregisterShapesObserver(obs: List[Shape] => Unit) =
    shapesObservers = shapesObservers filterNot (_ == obs)
  
  def unregisterStrokeWidthObserver(obs: Int => Unit) =
    strokeWidthObservers = strokeWidthObservers filterNot (_ == obs)
  
  def unregisterColorObserver(obs: Color => Unit) =
    colorObservers = colorObservers filterNot (_ == obs)
  
  def unregisterCommandsObserver(obs: List[Command] => Unit) =
    commandsObservers = commandsObservers filterNot (_ == obs)
  
  def unregisterFileNameObserver(obs: String => Unit) =
    fileNameObservers = fileNameObservers filterNot (_ == obs)
}


class NetworkSpaceState(
    val drawingStateSpace: DrawingSpaceState,
    val shapeUpdateRunner: (=> Unit) => Unit,
    val serverHostname: String = "localhost",
    val commandPort: Int = 9998,
    val exchangePort: Int = 9999,
    val listenerPort: Int = 1337) {
  val serverInetAddress: InetAddress = InetAddress.getByName(serverHostname)
  
  // Register this client with a server and tell it
  // which port the server has to send updates to
  {
    val socket = new Socket(serverInetAddress, commandPort)
    val out = new PrintWriter(socket.getOutputStream, true)
    out.println("register %d" format listenerPort)
    out.close
    socket.close
  }
  
  // listen for updates and send updates
  private val listener = new ServerSocket(listenerPort)
  private var updating = false
  new Actor {
    def act {
      println("start UpdateThread")
      try
        while (true) {
          println("receiving update")
          val socket = listener.accept
          val shapes = Shape.deserialize(XML.load(socket.getInputStream), drawingStateSpace)
          shapeUpdateRunner {
            updating = true
            drawingStateSpace.clear
            for (shape <- shapes)
              drawingStateSpace execute new CreateShape(shape)
            updating = false
          }
          socket.close
        }
      catch {
        case e: SocketException =>
      }
    }
  }.start
  
  drawingStateSpace.registerShapesObserver{ shapes =>
    if (!updating) {
      println("sending update")
      val socket = new Socket(serverInetAddress, exchangePort)
      val writer = new OutputStreamWriter(socket.getOutputStream)
      val port = Attribute(None, "port", Text(listenerPort.toString), Null)
      XML.write(writer, Shape.serialize(shapes) % port, "", false, null)
      writer.close
      socket.close
    }
  }
  
  def dispose = listener.close
}
