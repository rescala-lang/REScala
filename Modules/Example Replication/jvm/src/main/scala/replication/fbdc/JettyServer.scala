package replication.fbdc

import loci.communicator.ws.jetty.*
import loci.communicator.ws.jetty.WS.Properties
import loci.registry.Registry
import org.eclipse.jetty.server.handler.{ContextHandler, ResourceHandler}
import org.eclipse.jetty.server.{Handler, Request, Response, Server, ServerConnector}
import org.eclipse.jetty.util.Callback
import org.eclipse.jetty.util.resource.ResourceFactory
import org.eclipse.jetty.util.thread.QueuedThreadPool
import org.eclipse.jetty.websocket.server.WebSocketUpgradeHandler

import java.nio.file.Path
import scala.concurrent.duration.*

class JettyServer(
    staticPath: Option[Path],
    contextPath: String,
    registry: Registry,
    interface: String,
) {

  lazy val jettyServer: Server = {
    val threadPool = new QueuedThreadPool(3, 0)
    threadPool.setName("http server")
    new Server(threadPool)
  }

  def stop(): Unit = jettyServer.stop()

  def start(port: Int): Unit = {

    // connectors accept requests – in this case on a TCP socket
    val connector = new ServerConnector(jettyServer, 0, 1)
    connector.setHost(interface)
    connector.setPort(port)
    jettyServer.addConnector(connector)

    jettyServer.setHandler(new Handler.Sequence(mainHandler, staticResourceHandler, setupLociWebsocketContextHandler()))
    jettyServer.start()
  }

  val staticResourceHandler = {
    // Create and configure a ResourceHandler.
    val handler = new ResourceHandler()
    // Configure the directory where static resources are located.
    staticPath match
      case None       =>
      case Some(path) => handler.setBaseResource(ResourceFactory.of(handler).newResource(path))
    // Configure directory listing.
    handler.setDirAllowed(false)
    // Configure whether to accept range requests.
    handler.setAcceptRanges(true)
    handler
  }

  def setupLociWebsocketContextHandler() = {

    val contextHandler   = new ContextHandler()
    val webSocketHandler = WebSocketUpgradeHandler.from(jettyServer, contextHandler)

    // add loci registry
    val wspath     = "/ws"
    val properties = Properties(heartbeatDelay = 3.seconds, heartbeatTimeout = 10.seconds)
    registry.listen(WS(webSocketHandler, wspath, properties))

    contextHandler.setContextPath(contextPath)
    contextHandler.setHandler(webSocketHandler)

    contextHandler
  }

  object mainHandler extends Handler.Abstract {
    override def handle(
        request: Request,
        response: Response,
        callback: Callback
    ): Boolean = false

  }

}
