package channel.tcp

import channel.{Abort, ArrayMessageBuffer, ConnectionContext, Incoming, LatentConnection, MessageBuffer, OutChan}
import de.rmgk.delay
import de.rmgk.delay.{Async, Callback}

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, IOException}
import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket, SocketException}
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object TCP {

  inline def syncAttempt[T](inline x: T): Async[Any, T] = Async.fromCallback {
    try Async.handler.succeed(x)
    catch case NonFatal(exception) => Async.handler.fail(exception)
  }

  def handleConnection(socket: Socket, incoming: Incoming, executionContext: ExecutionContext): TCPConnection = {
    val conn = new TCPConnection(socket)
    executionContext.execute: () =>
      conn.handleReceivedMessages(incoming(conn))
    conn
  }

  def connect(host: String, port: Int, executionContext: ExecutionContext): LatentConnection =
    new LatentConnection {
      override def prepare(incoming: Incoming): Async[Any, ConnectionContext] =
        TCP.syncAttempt {
          TCP.handleConnection(new Socket(host, port), incoming, executionContext)
        }
    }

  def listen(interface: String, port: Int, executionContext: ExecutionContext): LatentConnection =
    new LatentConnection {
      override def prepare(incoming: ConnectionContext => delay.Callback[MessageBuffer])
          : Async[Abort, ConnectionContext] =
        Async.fromCallback { abort ?=>
          try
            val socket = new ServerSocket

            try socket.setReuseAddress(true)
            catch {
              case _: SocketException =>
              // some implementations may not allow SO_REUSEADDR to be set
            }

            socket.bind(new InetSocketAddress(InetAddress.getByName(interface), port))

            executionContext.execute { () =>
              try
                while (!abort.closeRequest) {
                  val connection = socket.accept()
                  if connection != null
                  then
                    Async.handler.succeed {
                      TCP.handleConnection(connection, incoming, executionContext)
                    }
                }
              catch {
                case exception: SocketException =>
                  Async.handler.fail(exception)
              }
            }

          catch
            case NonFatal(ex) => Async.handler.fail(ex)
        }
    }

}

class TCPConnection(socket: Socket) extends OutChan with ConnectionContext {

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

  // frame parsing

  def handleReceivedMessages(handler: Callback[MessageBuffer]) = {

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

            handler.succeed(ArrayMessageBuffer(bytes))

          case _ =>
            throw IOException("unexpected read")
        }
      }
    catch {
      case ioe: IOException =>
        close()
        handler.fail(ioe)
    }
  }

}
