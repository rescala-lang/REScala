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

import rescala.events.ImperativeEvent
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
  
  val nextShapeChanged = new ImperativeEvent[Shape] //#EVT
  val selectedShapeChanged = new ImperativeEvent[Shape] //#EVT
  val shapesChanged = new ImperativeEvent[List[Shape]] //#EVT
  val strokeWidthChanged = new ImperativeEvent[Int] //#EVT
  val colorChanged = new ImperativeEvent[Color] //#EVT
  val commandsChanged = new ImperativeEvent[List[Command]] //#EVT
  val fileNameChanged = new ImperativeEvent[String] //#EVT
  
  def execute(command: Command) {
    _commands ::= command
    commandsChanged(_commands)
    
    val shapes = command execute _shapes
    if (shapes != _shapes) {
      if (_selectedShape != null && !(shapes contains _selectedShape)) {
        _selectedShape = null
        selectedShapeChanged(_selectedShape)
      }
      
      _shapes = shapes
      shapesChanged(_shapes)
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
          selectedShapeChanged(_selectedShape)
        }
        
        _shapes = shapes
        shapesChanged(_shapes)
      }
      
      _commands = _commands drop count
      commandsChanged(_commands)
    }
  }
  
  def clear() =
    if (_shapes.nonEmpty) {
      _shapes = List.empty
      shapesChanged(_shapes)
      
      _commands = List.empty
      commandsChanged(_commands)
    }
    
  def nextShape_=(shape: Shape) =
    if (_nextShape != shape) {
      _nextShape = shape
      nextShapeChanged(_nextShape)
    }
  
  def selectedShape_=(shape: Shape) =
    if (_selectedShape != shape && (shape == null || (_shapes contains shape))) {
      _selectedShape = shape
      selectedShapeChanged(_selectedShape)
    }
  
  def strokeWidth_=(width: Int) =
    if (_strokeWidth != width) {
      _strokeWidth = width
      strokeWidthChanged(width)
    }
  
  def color_=(color: Color) =
    if (_color != color) {
      _color = color
      colorChanged(color)
    }
  
  def fileName_=(fileName: String) =
    if (_fileName != fileName) {
      _fileName = fileName
      fileNameChanged(fileName)
    }
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
  
  drawingStateSpace.shapesChanged += { shapes => //#HDL
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
