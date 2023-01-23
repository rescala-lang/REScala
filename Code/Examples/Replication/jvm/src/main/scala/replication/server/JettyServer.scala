package replication.server

import loci.communicator.ws.jetty.*
import loci.communicator.ws.jetty.WS.Properties
import jakarta.servlet.http.{Cookie, HttpServletRequest, HttpServletResponse}

import loci.registry.Registry
import org.eclipse.jetty.http.{HttpCookie, HttpHeader, HttpMethod}
import org.eclipse.jetty.rewrite.handler.{RewriteHandler, RewriteRegexRule}
import org.eclipse.jetty.server.handler.gzip.GzipHandler
import org.eclipse.jetty.server.handler.{AbstractHandler, HandlerList, HandlerWrapper, ResourceHandler}
import org.eclipse.jetty.server.{Request, Server, ServerConnector}
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.util.resource.Resource
import org.eclipse.jetty.util.thread.QueuedThreadPool
import rescala.default.Signal

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.Base64
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.concurrent.{Await, Promise}
import scala.jdk.CollectionConverters.*

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

    jettyServer.setHandler(new HandlerList(mainHandler, staticResourceHandler, setupLociWebsocketContextHandler()))
    jettyServer.start()
  }

  val staticResourceHandler = {
    // Create and configure a ResourceHandler.
    val handler = new ResourceHandler()
    // Configure the directory where static resources are located.
    staticPath match
      case None       =>
      case Some(path) => handler.setBaseResource(Resource.newResource(path))
    // Configure directory listing.
    handler.setDirectoriesListed(false)
    // Configure whether to accept range requests.
    handler.setAcceptRanges(true)
    handler
  }

  def setupLociWebsocketContextHandler() = {

    // define a context with a given prefix to add loci socket
    val context = new ServletContextHandler(ServletContextHandler.SESSIONS)
    context.setContextPath(contextPath)

    // add loci registry
    val wspath     = "/ws"
    val properties = Properties(heartbeatDelay = 3.seconds, heartbeatTimeout = 10.seconds)
    registry.listen(WS(context, wspath, properties))

    context
  }

  object mainHandler extends AbstractHandler {
    override def handle(
        target: String,
        baseRequest: Request,
        request: HttpServletRequest,
        response: HttpServletResponse
    ): Unit = {}

  }

}
