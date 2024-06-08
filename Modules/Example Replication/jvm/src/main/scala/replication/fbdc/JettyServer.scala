package replication.fbdc

import channel.jettywebsockets.JettyWsListener
import org.eclipse.jetty.http.pathmap.PathSpec
import org.eclipse.jetty.server.handler.ResourceHandler
import org.eclipse.jetty.server.{Handler, Request, Response, Server, ServerConnector}
import org.eclipse.jetty.util.Callback
import org.eclipse.jetty.util.resource.ResourceFactory
import org.eclipse.jetty.util.thread.QueuedThreadPool
import replication.DataManager

import java.nio.file.Path
import scala.util.{Failure, Success}

class JettyServer(
    staticPath: Option[Path],
    contextPath: String,
    dataManager: DataManager[?],
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

    val setup = JettyWsListener.fromServer(jettyServer)

    jettyServer.setHandler(new Handler.Sequence(setup.handlers, staticResourceHandler))

    dataManager.addLatentConnection(setup.listen(PathSpec.from("/ws")))

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

  object mainHandler extends Handler.Abstract {
    override def handle(
        request: Request,
        response: Response,
        callback: Callback
    ): Boolean = false

  }

}
