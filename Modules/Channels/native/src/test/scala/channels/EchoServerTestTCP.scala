package channels

import java.net.{InetAddress, InetSocketAddress, ServerSocket, SocketException}

class EchoServerTestTCP extends EchoCommunicationTest(
      { ec =>
        val socket = new ServerSocket

        try socket.setReuseAddress(true)
        catch {
          case _: SocketException =>
          // some implementations may not allow SO_REUSEADDR to be set
        }

        socket.bind(new InetSocketAddress(InetAddress.getByName("localhost"), 0))

        val port = socket.getLocalPort
        (port, TCP.listen(() => socket, ec))
      },
      ec => port => TCP.connect(TCP.defaultSocket(new InetSocketAddress("localhost", port)), ec)
    )
