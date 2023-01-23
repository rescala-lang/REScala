package reswing.reshapes.drawing

import java.awt.Color
import java.io.{OutputStreamWriter, PrintWriter}
import java.net.{InetAddress, ServerSocket, Socket, SocketException}

import rescala.default._
import reswing.reshapes.figures.{Line, Shape}

import scala.xml.{Attribute, Null, Text, XML}

/** Represents the current state of one drawing space */
class DrawingSpaceState {
  // selected shape to be drawn
  lazy val nextShape: Signal[Shape] = Signal[Shape] { new Line(this) } // #SIG
  // currently selected shape inside the drawing space
  final private val _selectedShape: Signal[Shape] = // #SIG
    ((shapes.changed && { shapes => // #IF  //#EF
      !(shapes contains selectedShape.value)
    } map { (_: Any) => null }) ||
      (select && { shape => // #EF
        shape == null || (shapes.value contains shape)
      })) latest null // #IF
  // Without this indirection, the access above is made static, which causes an immediate infinite recursion
  def selectedShape = _selectedShape
  // currently drawn shapes
  final lazy val shapes: Signal[List[Shape]] = Signal { commandsShapes() match { case (_, shapes) => shapes } } // #SIG
  // all executed commands
  final lazy val commands: Signal[List[Command]] =
    Signal { commandsShapes() match { case (commands, _) => commands } } // #SIG
  // current stroke width
  lazy val strokeWidth = Signal { 1 } // #SIG
  // current stroke color
  lazy val color = Signal { Color.BLACK } // #SIG
  // filename after saving
  val fileName = Var("unnamed") // #VAR

  // can be overridden in order to declare events declaratively
  lazy val executed: Event[Command] = Evt[Command]() // #EVT
  lazy val reverted: Event[Command] = Evt[Command]() // #EVT

  // events that can be called imperatively
  final lazy val execute = Evt[Command]() // #EVT
  final lazy val revert  = Evt[Command]() // #EVT
  final lazy val clear   = Evt[Unit]()    // #EVT
  final lazy val select  = Evt[Shape]()   // #EVT

  private sealed abstract class CommandType
  private case class Execute(command: Command) extends CommandType
  private case class Revert(command: Command)  extends CommandType
  private case class Clear()                   extends CommandType

  private lazy val commandInvoked: Event[CommandType] =
    ((executed || execute) map { (command: Command) => Execute(command) }) || // #EF //#EF //#EF
    ((reverted || revert) map { (command: Command) => Revert(command) }) ||   // #EF //#EF //#EF
    (clear map { (_: Unit) => Clear() })                                      // #EF

  private lazy val commandsShapes: Signal[(List[Command], List[Shape])] = // #SIG
    commandInvoked.fold((List.empty[Command], List.empty[Shape])) { // #IF
      case ((commands, shapes), commandType) => commandType match {
          case Execute(command) =>
            (command :: commands, command execute shapes)
          case Revert(command) =>
            commands indexOf command match {
              case -1 => (commands, shapes)
              case index =>
                val count = index + 1
                (
                  commands drop count,
                  (commands take count).foldLeft(shapes) { (shapes, command) => command revert shapes }
                )
            }
          case Clear() =>
            (List.empty, List.empty)
        }
    }
}

class NetworkSpaceState(
    val drawingStateSpace: DrawingSpaceState,
    val shapeUpdateRunner: (=> Unit) => Unit,
    val serverHostname: String = "localhost",
    val commandPort: Int = 9998,
    val exchangePort: Int = 9999,
    val listenerPort: Int = 1337
) {
  val serverInetAddress: InetAddress = InetAddress.getByName(serverHostname)

  // Register this client with a server and tell it
  // which port the server has to send updates to
  {
    val socket = new Socket(serverInetAddress, commandPort)
    val out    = new PrintWriter(socket.getOutputStream, true)
    out.println(s"register $listenerPort")
    out.close()
    socket.close()
  }

  // listen for updates and send updates
  private val listener = new ServerSocket(listenerPort)
  private var updating = false
  new Thread(new Runnable {
    override def run(): Unit = {
      println("start UpdateThread")
      try while (true) {
          println("receiving update")
          val socket = listener.accept
          val shapes = Shape.deserialize(XML.load(socket.getInputStream), drawingStateSpace)
          shapeUpdateRunner {
            updating = true
            drawingStateSpace.clear.fire()
            for (shape <- shapes)
              drawingStateSpace.execute.fire(new CreateShape(shape))
            updating = false
          }
          socket.close()
        }
      catch {
        case e: SocketException =>
      }
    }
  }).start()

  drawingStateSpace.shapes.changed += { shapes => // #IF //#HDL
    if (!updating) {
      println("sending update")
      val socket = new Socket(serverInetAddress, exchangePort)
      val writer = new OutputStreamWriter(socket.getOutputStream)
      val port   = Attribute(None, "port", Text(listenerPort.toString), Null)
      XML.write(writer, Shape.serialize(shapes) % port, "", false, null)
      writer.close()
      socket.close()
    }
  }

  def dispose(): Unit = listener.close()
}
