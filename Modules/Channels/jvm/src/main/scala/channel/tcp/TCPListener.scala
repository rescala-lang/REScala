package channel.tcp

import de.rmgk.delay.{Async, Sync}

import java.net.{InetAddress, InetSocketAddress, ServerSocket, SocketException}

class TCPListener(val socket: ServerSocket) {

  def connections: Async[Any, TCPConnection] = Async.fromCallback {

    try
      while (true) {
        val connection = socket.accept()
        if connection != null then Async.handler.succeed(new TCPConnection(connection))
      }
    catch {
      case exception: SocketException =>
        Async.handler.fail(exception)
    }
  }
}

object TCPListener {

  def startListening(port: Int, interface: String): TCPListener = {

    val socket = new ServerSocket

    try socket.setReuseAddress(true)
    catch {
      case _: SocketException =>
      // some implementations may not allow SO_REUSEADDR to be set
    }

    socket.bind(new InetSocketAddress(InetAddress.getByName(interface), port))

    new TCPListener(socket)

  }
}
