package reshapes.network
import java.net._
import java.io._
import scala.collection.mutable.MutableList
import reshapes.figures.Drawable
import scala.actors.Actor
import scala.actors.Actor._
import reshapes.command.Command

object ReshapesServer {

  var clients = MutableList[(InetAddress, Int)]()
  var currentShapes = List[Drawable]()

  def main(args: Array[String]): Unit = {
    new CommandThread(9998).start()
    new UpdateThread(9999).start()
  }

  /**
   * Registers a client to the server if not already registered.
   */
  def registerClient(inetAddress: InetAddress, port: Int) = {
    if (!(clients exists (client => client._1 == inetAddress && client._2 == port))) {
      clients += ((inetAddress, port))
      println("ReshapesServer register new client (%s, %d)".format(inetAddress, port))
      print("\t registered clients: ")
      clients map (client => println("(%s, %d)".format(client._1, client._2)))
      print("\n")
      sendToClient(currentShapes, (inetAddress, port))
    }
  }

  def removeClient(client: (InetAddress, Int)) = {
    println("ReshapesServer removing client " + client.toString())
    clients = clients filter (c => c._1 != client._1 && c._2 != client._2)
  }

  /**
   * Sends the given shapes to all registered clients except the original sender
   */
  def sendUpdateToClients(shapes: List[Drawable], sender: (InetAddress, Int)) = {
    currentShapes = shapes
    for (client <- clients) {
      if (client._1 != sender._1 ||
        (client._1 == sender._1 && client._2 != sender._2)) {
        if (!sendToClient(shapes, client)) {
          removeClient(sender)
        }
      }
    }
  }

  /**
   * Sends shapes to a client.
   * returns true if shapes where successfully send, false otherwise (connection refused to client)
   */
  def sendToClient(shapes: List[Drawable], client: (InetAddress, Int)): Boolean = {
    try {
      val socket = new Socket(client._1, client._2)
      val out = new ObjectOutputStream(new DataOutputStream(socket.getOutputStream()))

      out.writeObject(shapes)

      out.close()
      socket.close()
    } catch {
      case e: ConnectException =>
        return false
    }

    true
  }
}

/**
 * Listens to string commands:
 * 	register [port] - registers a new client
 */
class CommandThread(port: Int) extends Actor {
  def act() {
    println("start CommandThread")
    val listener = new ServerSocket(port)
    while (true) {
      val clientSocket = listener.accept()
      val in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()))

      val command = in.readLine()
      println("CommandThread new command: '%s'".format(command))
      command match {
        case registerCmd if registerCmd.toLowerCase().startsWith("register ") =>
          val clientPort = registerCmd.split(" ")(1).toInt
          ReshapesServer.registerClient(clientSocket.getInetAddress(), clientPort)
        case _ => println("unkown command: " + command)
      }

      in.close()
      clientSocket.close()
    }
    listener.close()
  }
}

/**
 * Listens to shapes updates
 */
class UpdateThread(port: Int) extends Actor {
  def act() {
    println("start UpdateThread")
    val listener = new ServerSocket(port)
    while (true) {
      val socket = listener.accept()
      val in = new ObjectInputStream(new DataInputStream(socket.getInputStream()));

      val shapes = in.readObject().asInstanceOf[TransportObject]

      ReshapesServer.sendUpdateToClients(shapes.shapes, (socket.getInetAddress(), shapes.senderPort))

      in.close()
      socket.close()
    }
    listener.close()
  }
}

