package channels

import de.rmgk.delay
import de.rmgk.delay.{Async, Callback}

import java.net.{InetSocketAddress, ServerSocket, Socket, SocketException}
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object TCP {

  inline def syncAttempt[T](inline x: T): Async[Any, T] = Async.fromCallback {
    try Async.handler.succeed(x)
    catch case NonFatal(exception) => Async.handler.fail(exception)
  }

  def handleConnection(
      socket: Socket,
      incoming: Receive[MessageBuffer],
      executionContext: ExecutionContext
  ): JIOStreamConnection = {
    println(s"handling new connection")
    socket.setTcpNoDelay(true)
    val conn = new JIOStreamConnection(socket.getInputStream, socket.getOutputStream, () => socket.close())
    executionContext.execute: () =>
      println(s"executing task")
      conn.loopHandler(incoming)
    conn
  }

  def defaultSocket(socketAddress: InetSocketAddress): () => Socket =
    () => new Socket(socketAddress.getAddress, socketAddress.getPort)

  def connect(bindsocket: () => Socket, executionContext: ExecutionContext): LatentConnection[MessageBuffer] =
    new LatentConnection {
      override def prepare(incoming: Receive[MessageBuffer]): Async[Any, Connection[MessageBuffer]] =
        TCP.syncAttempt {
          println(s"tcp sync attempt")
          TCP.handleConnection(bindsocket(), incoming, executionContext)
        }
    }

  def defaultServerSocket(socketAddress: InetSocketAddress): () => ServerSocket = () => {
    val socket = new ServerSocket

    try {
      socket.setReuseAddress(true)
    } catch {
      case _: SocketException =>
      // some implementations may not allow SO_REUSEADDR to be set
    }

    socket.bind(socketAddress)
    socket
  }

  def listen(bindsocket: () => ServerSocket, executionContext: ExecutionContext): LatentConnection[MessageBuffer] =
    new LatentConnection {
      override def prepare(incoming: Receive[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] =
        Async.fromCallback { abort ?=>
          try

            val socket = bindsocket()

            executionContext.execute { () =>
              try
                while !abort.closeRequest do {
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
