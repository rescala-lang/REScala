package channels

import channels.jettywebsockets.{JettyWsConnection, JettyWsListener}
import de.rmgk.delay.Async
import org.eclipse.jetty.http.pathmap.PathSpec

import java.net.{InetSocketAddress, URI}

class EchoServerTestJetty extends EchoCommunicationTest(
      _ =>
        (incoming: MessageBuffer.Handler) =>
          Async[Abort] {
            val listener   = JettyWsListener.prepareServer(54470)
            val echoServer = listener.listen(PathSpec.from("/registry/*"))
            listener.server.start()
            echoServer.prepare(incoming).bind
          },
      _ => JettyWsConnection.connect(URI.create(s"ws://localhost:54470/registry/"))
    )

class EchoServerTestUDP extends EchoCommunicationTest(
      UDP.sendreceive(InetSocketAddress("localhost", 54469), 54468, _),
      UDP.sendreceive(InetSocketAddress("localhost", 54468), 54469, _)
    )
