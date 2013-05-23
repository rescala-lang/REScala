package reshapes.drawing

import java.awt.Color
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.net.InetAddress
import java.net.ServerSocket
import java.net.Socket
import java.net.SocketException
import scala.actors.Actor
import scala.events.Event
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import scala.events.emptyevent
import scala.xml.Attribute
import scala.xml.Null
import scala.xml.Text
import scala.xml.XML
import reshapes.figures.Line
import reshapes.figures.Shape
import scala.events.ImperativeEvent

/**
 * Represents the current state of one drawing space
 */
class DrawingSpaceState {
  // selected shape to be drawn
  lazy val nextShape = Signal[Shape] { new Line(this) }
  // currently selected shape inside the drawing space
  private val _selectedShape = Var[Shape](null)
  final val selectedShape = Signal { _selectedShape() }
  // currently drawn shapes
  private val _shapes = Var(List.empty[Shape])
  final val shapes = Signal { _shapes() }
  // current stroke width
  lazy val strokeWidth = Signal { 1 }
  // current stroke color
  lazy val color = Signal { Color.BLACK }
  // all executed commands
  private val _commandsVar = Var(List.empty[Command])
  final val commands = Signal { _commandsVar() }
  // filename after saving
  val fileName = Var("unnamed")
  
  lazy val executed: Event[Command] = emptyevent
  lazy val reverted: Event[Command] = emptyevent
  
  final val execute = new ImperativeEvent[Command]
  final val revert = new ImperativeEvent[Command]
  final val select = new ImperativeEvent[Shape]
  
  executed || execute += { command =>
    _commandsVar() ::= command
    
    val shapes = command execute _shapes()
    if (shapes != _shapes) {
      if (_selectedShape != null && !(shapes contains _selectedShape))
        _selectedShape() = null
      
      _shapes() = shapes
    }
  }
  
  reverted || revert += { command =>
    val count = (_commandsVar.getValue indexOf command) + 1
    if (count != 0) {
      val shapes = (_shapes() /: (_commandsVar.getValue take count)) {
        (shapes, command) => command revert shapes
      }
      
      if (shapes != _shapes) {
        if (_selectedShape != null && !(shapes contains _selectedShape))
          _selectedShape() = null
        
        _shapes() = shapes
      }
      
      _commandsVar() = _commandsVar.getValue drop count
    }
  }
  
  def clear() =
    if (_shapes().nonEmpty) {
      _shapes() = List.empty
    }
  
  select += { shape =>
    if (_selectedShape() != shape && (shape == null || (_shapes() contains shape))) {
      _selectedShape() = shape
    }
  }
}

class NetworkSpaceState(
    val drawingStateSpace: DrawingSpaceState,
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
    def act() {
      println("start UpdateThread")
      try {
        while (true) {
          println("receiving update")
          val socket = listener.accept
          val shapes = Shape.deserialize(XML.load(socket.getInputStream), drawingStateSpace)
          updating = true
          drawingStateSpace.clear
          for (shape <- shapes)
            drawingStateSpace execute new CreateShape(shape)
          updating = false
          socket.close
        }
      }
      catch {
        case e: SocketException =>
      }
    }
  }.start
  
  drawingStateSpace.shapes.changed += { shapes =>
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
