package dtn

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, EOFException, IOException}
import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket, SocketException}
import java.util.concurrent.{ConcurrentHashMap, LinkedBlockingQueue}
import java.nio.charset.StandardCharsets

class TCPConnection(socket: Socket) {
  val inputStream  = new DataInputStream(new BufferedInputStream(socket.getInputStream))
  val outputStream = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))

  val remoteHostName: String = socket.getInetAddress.getHostName

  def send(data: Array[Byte]): Unit = {
    try {
      outputStream.writeInt(data.length)
      outputStream.write(data)
      outputStream.flush()
    } catch {
      case e: IOException => println(s"could not send data: $e"); throw e
    }
  }

  def close(): Unit = socket.close()

  def receive: Array[Byte] = {
    val size = inputStream.readInt()

    val bytes = new Array[Byte](size)

    inputStream.readFully(bytes, 0, size)

    println(s"I read this array of size ${size} with contents: ${bytes.toList}")

    bytes
  }
}
object TCPConnection {
  def apply(host: String, port: Int): TCPConnection = {
    new TCPConnection(new Socket(host, port))
  }
}

class TCPReadonlyServer(socket: ServerSocket) {
  // this queue must be read externally in regular intervals to not block the senders
  val queue: LinkedBlockingQueue[Tuple2[TCPConnection, Array[Byte]]] = new LinkedBlockingQueue(2000)

  val runnables: ConcurrentHashMap[TCPConnection, ReceiverRunnable] = ConcurrentHashMap()
  var listenerRunnable: Option[ListenerRunnable]                    = None

  def start(): Unit = {
    if listenerRunnable.nonEmpty then throw Exception("cannot start server twice")

    listenerRunnable = Option(ListenerRunnable())

    new Thread(listenerRunnable.get).start()
  }

  def stop(): Unit = {
    println("initiated tcp server stop")

    listenerRunnable match
      case None        => println("server was never started")
      case Some(value) => value.keepRunning = false

    runnables.values().forEach((receiverRunnable: ReceiverRunnable) => receiverRunnable.keepRunning = false)
    runnables.keySet().forEach((connection: TCPConnection) => connection.close())
    socket.close()

    println("requested all threads to stop and closed sockets")
  }

  class ListenerRunnable extends Runnable {
    var keepRunning: Boolean = true

    override def run(): Unit = {
      try {
        while keepRunning do {
          val connection = new TCPConnection(socket.accept())

          val receiverRunnable = ReceiverRunnable(connection)

          runnables.put(connection, receiverRunnable)

          new Thread(receiverRunnable).start()

          println(s"added new connection: ${connection.remoteHostName}")
        }
      } catch {
        case e: SocketException => println(s"server socket accept failed: $e")
      }
    }
  }

  class ReceiverRunnable(connection: TCPConnection) extends Runnable {
    var keepRunning: Boolean = true

    override def run(): Unit = {
      try {
        while keepRunning do {
          queue.put((connection, connection.receive))
        }
      } catch {
        case e: IOException  => println(s"read attempted on closed socket: $e")
        case e: EOFException => println(s"socket closed down while reading: $e")
      }
      println("closing socket")
      connection.close()
    }
  }
}
object TCPReadonlyServer {
  def apply(interface: String, port: Int): TCPReadonlyServer = {
    val socket = new ServerSocket

    try socket.setReuseAddress(true)
    catch {
      case _: SocketException =>
      // some implementations may not allow SO_REUSEADDR to be set
    }

    socket.bind(new InetSocketAddress(InetAddress.getByName(interface), port))

    new TCPReadonlyServer(socket)
  }
}
