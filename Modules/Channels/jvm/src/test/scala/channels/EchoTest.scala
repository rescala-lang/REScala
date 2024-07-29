package channels

import channels.jettywebsockets.{JettyWsConnection, JettyWsListener}
import com.sun.net.httpserver.HttpServer
import org.eclipse.jetty.http.pathmap.PathSpec
import org.eclipse.jetty.server.ServerConnector
import rdts.base.LocalUid

import java.net.http.HttpClient
import java.net.{DatagramSocket, InetSocketAddress, URI}

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
