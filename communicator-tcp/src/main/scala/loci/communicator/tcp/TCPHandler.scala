package loci
package communicator
package tcp

import java.io.{BufferedInputStream, BufferedOutputStream, IOException}
import java.net.{DatagramSocket, InetAddress, InetSocketAddress, ServerSocket, Socket, SocketException}
import java.util.concurrent.{Executors, ScheduledFuture, ThreadFactory, TimeUnit}
import scala.collection.mutable
import scala.util.{Failure, Success}
import scala.util.control.NonFatal
import de.rmgk.delay.Async

trait MessageBuffer {
  def asArray: Array[Byte]
  def length: Int
}

case class ArrayMessageBuffer(inner: Array[Byte]) extends MessageBuffer {
  override def asArray: Array[Byte] = inner
  override def length: Int          = inner.length
}

trait Channel {
  def close(): Async[Any, Unit]
  def closed: Async[Any, Boolean]
}

trait InChan extends Channel {
  def receive: Async[Any, MessageBuffer]
}

trait OutChan extends Channel {
  def send(message: MessageBuffer): Async[Any, Unit]

}

case class Bidirectional(in: InChan, out: OutChan)

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

  val inputStream  = new BufferedInputStream(socket.getInputStream)
  val outputStream = new BufferedOutputStream(socket.getOutputStream)

  // control codes

  val head: Byte    = 1
  val payload: Byte = 2

  // connection interface

  def send(data: MessageBuffer): Async[Any, Unit] = Async.fromCallback {
    try {
      val size = data.length
      outputStream.write(
        Array(
          head,
          (size >> 24).toByte,
          (size >> 16).toByte,
          (size >> 8).toByte,
          size.toByte,
          payload
        )
      )
      outputStream.write(data.asArray)
      outputStream.flush()
      Async.handler.succeed(())
    } catch {
      case ioe: IOException => Async.handler.fail(ioe)
    }
  }

  def close(): Async[Any, Unit] = Async { doClose() }

  def doClose(): Unit = {
    def ignoreIOException(body: => Unit) =
      try body
      catch { case _: IOException => }

    ignoreIOException { socket.shutdownOutput() }
    ignoreIOException { while (inputStream.read != -1) {} }
    ignoreIOException { socket.close() }
  }

  override def closed: Async[Any, Boolean] = Async(socket.isClosed)

  // heartbeat

  // socket.setSoTimeout(timeout)

  // frame parsing

  override def receive: Async[Any, MessageBuffer] = Async.fromCallback {

    def read = {
      val byte = inputStream.read
      if byte == -1
      then throw new IOException("end of connection stream")
      else byte.toByte
    }

    val arrayBuilder = mutable.ArrayBuilder.make[Byte]

    try while (true) {
        read match {
          case `head` =>
            var size =
              ((read & 0xff) << 24) |
              ((read & 0xff) << 16) |
              ((read & 0xff) << 8) |
              (read & 0xff)

            if (read == payload && size >= 0) {
              arrayBuilder.clear()
              arrayBuilder.sizeHint(size)
              while (size > 0) {
                arrayBuilder += read
                size -= 1
              }

              Async.handler.succeed(ArrayMessageBuffer(arrayBuilder.result()))
            } else
              throw IOException("unexpected read")

          case _ =>
            throw IOException("unexpected read")
        }
      }
    catch {
      case ioe: IOException =>
        doClose()
        Async.handler.fail(ioe)
    }
  }

}

trait TCPListener {
  def channels: Async[Any, Bidirectional]
  def close: Async[Any, Unit]
}

def startListening(port: Int, interface: String): TCPListener = {

  new TCPListener {

    val socket = new ServerSocket

    try socket.setReuseAddress(true)
    catch {
      case _: SocketException =>
      // some implementations may not allow SO_REUSEADDR to be set
    }

    override def close: Async[Any, Unit] = Async {
      socket.close()
    }

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
