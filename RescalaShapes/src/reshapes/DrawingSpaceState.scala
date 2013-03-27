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
import scala.events.behaviour.Var

import reshapes.command.Command
import reshapes.figures.Line
import reshapes.figures.Shape

/**
 * Represents the current state of one drawing space
 */
class DrawingSpaceState {

  // defines which shape is drawn next for example reshapes.figures.Line if nextShape contains reference to line
  val nextShape: Var[Shape] = new Var(new Line)
  // contains the currently selected shape which is, for example, moved around or edited (size/stroke)
  val selectedShape: Var[Shape] = new Var(null)
  // stores all currently drawn shapes
  val allShapes: Var[List[Shape]] = new Var(List[Shape]())
  // currently selected stroke width
  val strokeWidth: Var[Int] = new Var(1)
  // currently selected stroke color
  val color: Var[Color] = new Var(Color.BLACK)
  // stores all executed commands
  val Commands: Var[List[Command]] = new Var(List[Command]())
  // stores the filename after saving
  val fileName: Var[String] = new Var("unnamed")

  var mode: EditingMode = Drawing()

  val modeChange = nextShape.changed || selectedShape.changed

  val canvasChange = selectedShape.changed || allShapes.changed || modeChange || strokeWidth.changed || color.changed
}

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

/**
 * Listens for updates from server and updates events.allShapes
 */
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

abstract class EditingMode
case class Drawing extends EditingMode
case class Selection extends EditingMode