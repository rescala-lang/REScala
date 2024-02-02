package channel.tcp

import channel.{ArrayMessageBuffer, Bidirectional, InChan, MessageBuffer, OutChan}

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, IOException}
import java.net.{DatagramSocket, InetAddress, InetSocketAddress, ServerSocket, Socket, SocketException}
import java.util.concurrent.{Executors, ScheduledFuture, ThreadFactory, TimeUnit}
import scala.collection.mutable
import scala.util.{Failure, Success}
import scala.util.control.NonFatal
import de.rmgk.delay.Async

import java.nio.ByteBuffer

def connect(host: String, port: Int): Async[Any, Bidirectional] = Async.fromCallback {
  try
    Async.handler.succeed:
      makeConnection(new Socket(host, port))
  catch
    case NonFatal(exception) =>
      Async.handler.fail(exception)
}

def makeConnection(socket: Socket): Bidirectional =
  val conn = new TCPConnection(socket)
  Bidirectional(conn, conn)

class TCPConnection(socket: Socket) extends InChan with OutChan {

  // socket streams

  val inputStream  = new DataInputStream(new BufferedInputStream(socket.getInputStream))
  val outputStream = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))

  // control codes

  val sizedContent: Byte = 1

  // connection interface

  def send(data: MessageBuffer): Async[Any, Unit] = Async.fromCallback {
    try {
      val outArray = data.asArray
      outputStream.write(sizedContent)
      outputStream.writeInt(outArray.size)
      outputStream.write(outArray)
      outputStream.flush()
      Async.handler.succeed(())
    } catch {
      case ioe: IOException => Async.handler.fail(ioe)
    }
  }

  def close(): Unit = socket.close()

  // heartbeat

  // socket.setSoTimeout(timeout)

  // frame parsing

  override def receive: Async[Any, MessageBuffer] = Async.fromCallback {

    def readNextByte() = {
      val byte = inputStream.read
      if byte == -1
      then throw new IOException("end of connection stream")
      else byte.toByte
    }

    try while (true) {
        readNextByte() match {
          case `sizedContent` =>
            val size = inputStream.readInt()

            val bytes = new Array[Byte](size)
            inputStream.readFully(bytes, 0, size)

            Async.handler.succeed(ArrayMessageBuffer(bytes))

          case _ =>
            throw IOException("unexpected read")
        }
      }
    catch {
      case ioe: IOException =>
        close()
        Async.handler.fail(ioe)
    }
  }

}

trait TCPListener extends AutoCloseable {
  def channels: Async[Any, Bidirectional]
}

def startListening(port: Int, interface: String): TCPListener = {

  new TCPListener {

    val socket = new ServerSocket

    try socket.setReuseAddress(true)
    catch {
      case _: SocketException =>
      // some implementations may not allow SO_REUSEADDR to be set
    }

    override def close: Unit = socket.close()

    override def channels: Async[Any, Bidirectional] = Async.fromCallback {

      socket.bind(new InetSocketAddress(InetAddress.getByName(interface), port))

      try
        while (true) {
          val connection = socket.accept()
          if connection != null then Async.handler.succeed(makeConnection(connection))
        }
      catch {
        case exception: SocketException =>
          Async.handler.fail(exception)
      }
    }
  }
}
