package loci
package communicator
package tcp

import java.io.IOException
import java.net.{InetAddress, InetSocketAddress, ServerSocket, SocketException}
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

private class TCPListener(
  port: Int, interface: String, properties: TCP.Properties)
    extends Listener[TCP] {

  protected def startListening(connectionEstablished: Connected[TCP]): Try[Listening] =
    try {
      val running = new AtomicBoolean(true)
      val executor = Executors.newCachedThreadPool()
      val socket = new ServerSocket

      try socket.setReuseAddress(true) catch {
        case _: SocketException =>
          // some implementations may not allow SO_REUSEADDR to be set
      }

      socket.bind(new InetSocketAddress(InetAddress.getByName(interface), port))

      def terminate() = {
        try socket.close()
        catch { case _: IOException => }
        executor.shutdown()
      }

      new Thread() {
        override def run() =
          try
            while (true) {
              val connection = socket.accept()
              if (connection != null)
                executor.execute(new Runnable {
                  def run() = TCPHandler.handleConnection(
                    connection, properties, TCPListener.this, { connection =>
                      connectionEstablished.fire(Success(connection))
                    })
                })
            }
          catch {
            case exception: SocketException =>
              if (running.getAndSet(false)) {
                terminate()
                connectionEstablished.fire(Failure(exception))
              }
          }
      }.start()

      Success(new Listening {
        def stopListening(): Unit =
          if (running.getAndSet(false))
            terminate()
      })
    }
    catch {
      case NonFatal(exception) =>
        Failure(exception)
    }
}
