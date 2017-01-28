package reshapes

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.net.ConnectException
import java.net.InetAddress
import java.net.ServerSocket
import java.net.Socket

import scala.actors.Actor
import scala.xml.Elem
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.XML

object ReShapesServer {
  var clients: List[(InetAddress, Int)] = List.empty
  var currentShapes: Elem = null
  
  def main(args: Array[String]) {
    if (args.size >= 2) {
      val commandThreadPort = args(0).toInt
      val updateThreadPort = args(1).toInt
      
      new CommandThread(commandThreadPort).start
      new UpdateThread(updateThreadPort).start
    }
    else
      println("invalid number of arguments")
  }
  
  /**
   * Registers a client to the server if not already registered.
   */
  def registerClient(inetAddress: InetAddress, port: Int) =
    if (!(clients contains ((inetAddress, port)))) {
      clients ::= (inetAddress, port)
      println("ReshapesServer register new client (%s, %d)" format (inetAddress, port))
      println("\t registered clients: ")
      for (client <- clients)
        println("\t  (%s, %d)" format (client._1, client._2))
      println
      sendToClient((inetAddress, port))
    }
  
  /**
   * Removes a client so he no longer receives updates.
   */
  def removeClient(client: (InetAddress, Int)) {
    println("ReshapesServer removing client " + client.toString)
    clients = clients filterNot (_ == client)
  }
  
  /**
   * Sends the given shapes to all registered clients except the original sender
   */
  def sendUpdateToClients(shapes: Elem, sender: (InetAddress, Int)) {
    currentShapes = shapes
    for (client <- clients)
      if (client != sender && !sendToClient(client))
          removeClient(client)
  }
  
  /**
   * Sends shapes to a client.
   * returns true if shapes where successfully send, false otherwise (connection refused to client)
   */
  def sendToClient(client: (InetAddress, Int)) = {
    try {
      if (currentShapes != null) {
        val socket = new Socket(client._1, client._2)
        val writer = new OutputStreamWriter(socket.getOutputStream)
        XML.write(writer, currentShapes, "", false, null)
        writer.close
        socket.close
      }
      true
    }
    catch {
      case e: ConnectException => false
    }
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
      val clientSocket = listener.accept
      val in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream))
      
      val command = in.readLine()
      println("CommandThread new command: '%s'" format command)
      command match {
        case registerCmd if registerCmd.toLowerCase.startsWith("register ") =>
          val clientPort = registerCmd.split(" ")(1).toInt
          ReShapesServer.registerClient(clientSocket.getInetAddress, clientPort)
        case _ => println("unkown command: " + command)
      }
      
      in.close
      clientSocket.close
    }
    listener.close
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
      val socket = listener.accept
      val shapes = XML.load(socket.getInputStream)
      
      ReShapesServer.sendUpdateToClients(
          shapes.copy(attributes = shapes.attributes.remove("port")),
          (socket.getInetAddress, (shapes attribute "port").get.text.toInt))
      socket.close
    }
    listener.close
  }
}
