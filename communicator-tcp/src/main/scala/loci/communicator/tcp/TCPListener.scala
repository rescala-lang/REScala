package loci
package communicator
package tcp

import de.rmgk.delay.Async

import java.io.IOException
import java.net.{InetAddress, InetSocketAddress, ServerSocket, SocketException}
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

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
