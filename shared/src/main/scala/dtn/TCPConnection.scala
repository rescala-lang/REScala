package dtn


import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, IOException, EOFException}
import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket, SocketException}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.LinkedBlockingQueue


class TCPConnection(socket: Socket) {

  val inputStream  = new DataInputStream(new BufferedInputStream(socket.getInputStream))
  val outputStream = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))


  def send(data: Array[Byte]): Unit = {
    try {
      outputStream.writeInt(data.size)
      outputStream.write(data)
      outputStream.flush()
    } catch {
      case e: IOException => {println(s"could not send data: $e"); throw e}
    }
  }

  def close(): Unit = socket.close()

  def receive: Array[Byte] = {
    try {
      val size = inputStream.readInt()

      val bytes = new Array[Byte](size)

      inputStream.readFully(bytes, 0, size)

      bytes
    } catch {
      case e: IOException => {println(s"read attempted on closed socket: $e"); throw e}
      case e: EOFException => {println(s"socket closed down while reading: $e"); throw e}
    }
  }
}
object TCPConnection {
  def apply(host: String, port: Int): TCPConnection = {
    new TCPConnection(new Socket(host, port))
  }
}


class TCPServer(socket: ServerSocket) {
  // first queue: messages to send, second queue: messages received
  val connections: ConcurrentHashMap[TCPConnection, Tuple2[LinkedBlockingQueue[Array[Byte]], LinkedBlockingQueue[Array[Byte]]]] = ConcurrentHashMap()

  val runnables: ConcurrentHashMap[TCPConnection, Tuple2[SenderRunnable, ReceiverRunnable]] = ConcurrentHashMap()

  var listenerRunnable: Option[ListenerRunnable] = None


  def start(): Unit = {
    if (listenerRunnable.nonEmpty) throw Exception("cannot start server twice")

    listenerRunnable = Option(ListenerRunnable())

    println("starting listener")
    new Thread(listenerRunnable.get).start()
    println("started listener")
  }

  def stop(): Unit = {
    println("initiated tcp server stop")

    listenerRunnable match
      case None => println("server was never started")
      case Some(value) => value.keepRunning = false

    runnables.values().forEach((senderRunnable: SenderRunnable, receiverRunnable: ReceiverRunnable) => {
      senderRunnable.keepRunning = false
      receiverRunnable.keepRunning = false
    })

    runnables.keySet().forEach((connection: TCPConnection) => {
      connection.close()
    })

    socket.close()

    println("requested all threads to stop and closed sockets")
  }

  class ListenerRunnable extends Runnable {
    var keepRunning: Boolean = true

    override def run(): Unit = {
      try {
        while(keepRunning) {
          println("before accept")
          val connection = new TCPConnection(socket.accept())
          println("after accept")

          val senderQueue: LinkedBlockingQueue[Array[Byte]] = new LinkedBlockingQueue
          val receiverQueue: LinkedBlockingQueue[Array[Byte]] = new LinkedBlockingQueue
          val senderRunnable = SenderRunnable(connection, senderQueue)
          val receiverRunnable = ReceiverRunnable(connection, receiverQueue)

          runnables.put(connection, (senderRunnable, receiverRunnable))
          connections.put(connection, (senderQueue, receiverQueue))

          new Thread(senderRunnable).start()
          new Thread(receiverRunnable).start()
        }
      } catch {
        case e: SocketException => println(s"server socket accept failed: $e")
      }
    }
  }

  class ReceiverRunnable(connection: TCPConnection, queue: LinkedBlockingQueue[Array[Byte]]) extends Runnable {
    var keepRunning: Boolean = true

    override def run(): Unit = {
      println("receiver running")
      while(keepRunning) {
        queue.put(connection.receive)
        println("received message")
      }
    }
  }

  class SenderRunnable(connection: TCPConnection, queue: LinkedBlockingQueue[Array[Byte]]) extends Runnable {
    var keepRunning: Boolean = true

    override def run(): Unit = {
      while(keepRunning) {
        val data = queue.take()
        connection.send(data)
        println("sent message")
      }
    }
  }
}
object TCPServer {
  def apply(port: Int, interface: String): TCPServer = {
    val socket = new ServerSocket

    try socket.setReuseAddress(true)
    catch {
      case _: SocketException =>
      // some implementations may not allow SO_REUSEADDR to be set
    }

    socket.bind(new InetSocketAddress(InetAddress.getByName(interface), port))

    new TCPServer(socket)
  }
}



