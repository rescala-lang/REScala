package channels

import channels.jettywebsockets.{JettyWsConnection, JettyWsListener}
import de.rmgk.delay.Async
import org.eclipse.jetty.http.pathmap.PathSpec

import java.net.{DatagramSocket, InetSocketAddress, URI}

class EchoServerTestJetty extends EchoCommunicationTest(
      _ =>
        (
          None,
          (incoming: Handler[MessageBuffer]) =>
            Async[Abort] {
              val listener   = JettyWsListener.prepareServer(54470)
              val echoServer = listener.listen(PathSpec.from("/registry/*"))
              listener.server.start()
              echoServer.prepare(incoming).bind
            }
        ),
      _ => _ => JettyWsConnection.connect(URI.create(s"ws://localhost:54470/registry/"))
    )

class EchoServerTestUDP extends EchoCommunicationTest(
      ec => {
        // don’t do this normally, but we need a free random socket
        val ds = new DatagramSocket()
        (ds.getLocalPort, UDP.listen(() => ds, ec))
      },
      ec => info => UDP.connect(InetSocketAddress("localhost", info), () => new DatagramSocket(), ec)
    )
