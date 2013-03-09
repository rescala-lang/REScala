package reshapes
import scala.events.ImperativeEvent
import reshapes.figures.Shape
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
import reshapes.network.TransportObject

/**
 * Unifies all events which can occure during execution
 */
class Events {

  val nextShape: Var[Shape] = new Var(new Line)
  val selectedShape: Var[Shape] = new Var(null)
  val allShapes: Var[List[Shape]] = new Var(List[Shape]())
  val strokeWidth: Var[Int] = new Var(1)
  val color: Var[Color] = new Var(Color.BLACK)
  val Commands: Var[List[Command]] = new Var(List[Command]())
  val fileName: Var[String] = new Var("unnamed")

  var mode: EditingMode = Drawing()
  val modeChange = nextShape.changed || selectedShape.changed

  val canvasChange = selectedShape.changed || allShapes.changed || modeChange || strokeWidth.changed

  nextShape.changed += (shape => {
    shape.strokeWidth = strokeWidth.getValue
    shape.color = color.getValue
    allShapes.getValue map (x => x.selected = false)
    mode = Drawing()
  })

  selectedShape.changed += (shape => {
    if (shape != null) {
      allShapes.getValue map (x => x.selected = false)
      shape.selected = true
      mode = Selection()
    }
  })

  strokeWidth.changed += (width => {
    if (selectedShape.getValue != null) {
      selectedShape.getValue.strokeWidth = width
    }
  })

  color.changed += (newColor => {
    if (selectedShape.getValue != null) {
      selectedShape.getValue.color = newColor
    }
  })
}

class NetworkEvents(serverHostname: String = "localhost", commandPort: Int = 9998, exchangePort: Int = 9999, listenerPort: Int = 1337) extends Events {

  val serverInetAddress: InetAddress = InetAddress.getByName(serverHostname)

  val commandSignal = Signal { Commands() }

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

  val cmdFlow = scalareact.Signal.flow("") { self =>
    while (true) {
      self awaitNext commandSignal

      val socket = new Socket(serverInetAddress, exchangePort)
      val out = new ObjectOutputStream(new DataOutputStream(socket.getOutputStream()))

      out.writeObject(new TransportObject(allShapes.getValue, listenerPort))

      out.close()
      socket.close()
    }
  }

  registerClient(serverHostname, commandPort, listenerPort)
  startUpdateListener(listenerPort)
}

/**
 * Listens for updates from server and updates events.allShapes
 */
class UpdateListener(port: Int, events: Events) extends Actor {
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