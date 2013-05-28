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
import scala.events.ImperativeEvent
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import scala.events.emptyevent
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
  lazy val nextShape = Signal[Shape] { new Line(this) }
  // currently selected shape inside the drawing space
  final lazy val selectedShape: Signal[Shape] =
    ((shapes.changed && { shapes =>
       !(shapes contains selectedShape.getValue) } map {_: Any => null}) ||
     (select && { shape =>
       shape == null || (shapes.getValue contains shape) })) latest null
  // currently drawn shapes
  final lazy val shapes = Signal { commandsShapes() match { case (_, shapes) => shapes } }
  // all executed commands
  final lazy val commands = Signal { commandsShapes() match { case (commands, _) => commands } }
  // current stroke width
  lazy val strokeWidth = Signal { 1 }
  // current stroke color
  lazy val color = Signal { Color.BLACK }
  // filename after saving
  val fileName = Var("unnamed")
  
  // can be
  lazy val executed: Event[Command] = emptyevent
  lazy val reverted: Event[Command] = emptyevent
  
  final lazy val execute = new ImperativeEvent[Command]
  final lazy val revert = new ImperativeEvent[Command]
  final lazy val clear = new ImperativeEvent[Unit]
  final lazy val select = new ImperativeEvent[Shape]
  
  private lazy val commandsShapes: Signal[(List[Command], List[Shape])] =
    (((executed || execute) map { command: Command =>
        val _commands = command :: commands.getValue
        val _shapes = command execute shapes.getValue
        (_commands, _shapes)
      }) ||
     ((reverted || revert) map { command: Command =>
        val count = (commands.getValue indexOf command) + 1
        if (count != 0) {
          val _shapes = (shapes.getValue /: (commands.getValue take count)) {
            (shapes, command) => command revert shapes
          }
          (commands.getValue drop count, _shapes)
        }
        else
          (commands.getValue, shapes.getValue)
      }) ||
     (clear map { _: Unit =>
       (List.empty, List.empty)
     })) latest (List.empty, List.empty)
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
            drawingStateSpace.clear()
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
