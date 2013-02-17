package reshapes
import scala.events.ImperativeEvent
import reshapes.figures.Drawable
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import reshapes.figures.Line
import java.awt.Color
import reshapes.command.Command
import scala.events.scalareact
import java.net._
import java.io.ObjectOutputStream
import java.io.DataOutputStream
import java.io.ObjectInputStream
import java.io.DataInputStream
import reshapes.command.CreateShape
import java.io.IOException
import org.omg.CORBA.portable.OutputStream
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.PrintWriter
import scala.actors.Actor

/**
 * Unifies all events which can occure during execution
 */
class Events {

  val nextShape: Var[Drawable] = new Var(new Line)
  val selectedShape: Var[Drawable] = new Var(null)
  val allShapes: Var[List[Drawable]] = new Var(List[Drawable]())
  val strokeWidth: Var[Int] = new Var(1)
  val color: Var[Color] = new Var(Color.BLACK)
  val Commands: Var[List[Command]] = new Var(List[Command]())
  val fileName: Var[String] = new Var("unnamed")

  var mode: EditingMode = Drawing()
  val modeChange = nextShape.changed || selectedShape.changed

  val canvasChange = selectedShape.changed || allShapes.changed || modeChange

  nextShape.changed += (shape => {
    shape.strokeWidth = strokeWidth.getValue
    shape.color = color.getValue
    allShapes.getValue map (x => x.selected = false)
    mode = Drawing()
  })

  selectedShape.changed += (shape => {
    allShapes.getValue map (x => x.selected = false)
    shape.selected = true
    mode = Selection()
  })

  val e = new ImperativeEvent[Int]
  //val accum = e.fold(0) { _ + _ }
  val accum = Signal { strokeWidth() }

  val flow2 = scalareact.Signal.flow("No occurence") { self =>
    while (true) {
      self awaitNext accum
      println(accum.getValue)
    }
  }

}

class NetworkEvents(serverHostname: String = "localhost", serverPort: Int = 9998, listenerPort: Int = 1337) extends Events {

  val serverUpdatePort: Int = serverPort + 1
  val serverInetAddress: InetAddress = InetAddress.getByName(serverHostname)

  allShapes.changed += update

  /**
   * Registers this client with a server and tells him
   * which port the server has to send updates to
   */
  def registerClient(serverHostname: String, serverPort: Int, portToRegister: Int) = {
    try {
      val socket = new Socket(serverInetAddress, serverPort)
      val out = new PrintWriter(socket.getOutputStream(), true)

      out.println("register %d".format(portToRegister))

      out.close()
      socket.close()
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }

  def startUpdateListener(port: Int) = {
    new UpdateListener(port, this).start()
  }

  /**
   * Sends a upate to the server
   */
  def update(shapes: List[Drawable]) = {
    val socket = new Socket(serverInetAddress, serverUpdatePort)
    val out = new ObjectOutputStream(new DataOutputStream(socket.getOutputStream()))

    out.writeObject(shapes)

    out.close()
    socket.close()
  }

  // calls at startup
  registerClient(serverHostname, serverPort, listenerPort)
  startUpdateListener(listenerPort)
}

/**
 * Listens for updates from server and updates allShapes
 */
class UpdateListener(port: Int, events: Events) extends Actor {
  def act() {
    println("start UpdateThread")
    val listener = new ServerSocket(port)
    while (true) {
      val socket = listener.accept()
      val in = new ObjectInputStream(new DataInputStream(socket.getInputStream()));

      val shapes = in.readObject().asInstanceOf[List[Drawable]]
      syncShapes(shapes)

      in.close()
      socket.close()
    }
    listener.close()
  }

  def syncShapes(shapes: List[Drawable]) = {
    for (shape <- shapes) {
      if (!events.allShapes.getValue.contains(shape)) {
        println("adding shape " + shape.strokeWidth);
        events.allShapes() = shape :: events.allShapes.getValue
      }
    }

  }
}

abstract class EditingMode
case class Drawing extends EditingMode
case class Selection extends EditingMode