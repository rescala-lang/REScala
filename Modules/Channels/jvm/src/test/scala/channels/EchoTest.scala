package channels

import channels.jettywebsockets.{JettyWsConnection, JettyWsListener}
import com.sun.net.httpserver.HttpServer
import org.eclipse.jetty.http.pathmap.PathSpec
import org.eclipse.jetty.server.ServerConnector
import rdts.base.LocalUid

import java.net.http.HttpClient
import java.net.{DatagramSocket, InetSocketAddress, StandardProtocolFamily, URI, UnixDomainSocketAddress}
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.file.Files

class EchoServerTestJetty extends EchoCommunicationTest(
      { ec =>
        val listener   = JettyWsListener.prepareServer(0)
        val echoServer = listener.listen(PathSpec.from("/registry/*"))
        listener.server.start()
        val port = listener.server.getConnectors.head.asInstanceOf[ServerConnector].getLocalPort
        (port, echoServer)
      },
      _ => port => JettyWsConnection.connect(URI.create(s"ws://localhost:$port/registry/"))
    )

class EchoServerTestUDP extends EchoCommunicationTest(
      ec => {
        // don’t do this normally, but we need a free random socket
        val ds = new DatagramSocket()
        (ds.getLocalPort, UDP.listen(() => ds, ec))
      },
      ec => info => UDP.connect(InetSocketAddress("localhost", info), () => new DatagramSocket(), ec)
    )

class EchoServerTestSunJavaHTTP extends EchoCommunicationTest(
      ec => {

        val server = HttpServer.create()

        server.bind(InetSocketAddress("0", 58004), 0)

        val handler = JavaHttp.SSEServer(handler => {
          server.createContext("/path", handler)
        })

        server.start()
        val port = server.getAddress.getPort

        println(s"server started")

        (port, handler)

      },
      ec =>
        port => {
          val client = HttpClient.newHttpClient()
          JavaHttp.SSEClient(client, new URI(s"http://localhost:$port/path"), LocalUid.gen(), ec)
        }
    )

def domainSocketHelperNonensese(name: String) = {
  val tmpPath    = Files.createTempDirectory("channels-test")
  val socketPath = tmpPath.resolve(name)
  socketPath.toFile.deleteOnExit()
  tmpPath.toFile.deleteOnExit()
  UnixDomainSocketAddress.of(socketPath)
}

class EchoServerTestNioTCP extends EchoCommunicationTest(
      { ec =>
        val socket = ServerSocketChannel.open(StandardProtocolFamily.UNIX)

        socket.configureBlocking(false)

        // socket.bind(new InetSocketAddress("localhost", 0))

        val socketPath = domainSocketHelperNonensese("some-name")

        socket.bind(socketPath)

        // val port = socket.socket().getLocalPort

        println(s"server listening at")

        val nioTCP = new NioTCP

        ec.execute(() => nioTCP.loopSelection(Abort()))

        (socketPath, nioTCP.listen(() => socket))
      },
      ec =>
        sp => {

          def socketChannel: SocketChannel = {
            val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
            channel.connect(sp)
            channel.configureBlocking(false)
            channel
          }

          val nioTCP = new NioTCP

          ec.execute(() => nioTCP.loopSelection(Abort()))

          nioTCP.connect(() => socketChannel)
        }
    )
