package encrdtlib.sync.p2p

import encrdtlib.sync.p2p.P2PConnectionManager.REPLICAID_HEADER
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.websocket.server.config.JettyWebSocketServletContainerInitializer
import org.eclipse.jetty.websocket.server.{JettyServerUpgradeRequest, JettyServerUpgradeResponse, JettyWebSocketCreator}

import java.net.URI
import java.time.Duration

class CrdtSyncWebSocketServer[S](
    val localReplicaId: String,
    private val connectionManager: P2PConnectionManager[S],
    private val handlerFactory: String => CrdtSyncWebSocketHandler[S]
) {

  private val server    = new Server() // TODO: pass thread-pool?
  private val connector = new ServerConnector(server)
  server.addConnector(connector)

  private val ctxHandler = new ServletContextHandler()
  ctxHandler.setContextPath("/")
  server.setHandler(ctxHandler)

  private val webSocketCreator: JettyWebSocketCreator =
    (req: JettyServerUpgradeRequest, resp: JettyServerUpgradeResponse) => {
      println(s"trying to create handler for $req")

      val remoteReplicaId = req.getHeader(REPLICAID_HEADER)

      if (remoteReplicaId.forall(Character.isWhitespace)) {
        println(s"ReplicaId header is blank, refusing connection from ${req.getRemoteSocketAddress}")
        null
      } else {
        resp.setHeader(REPLICAID_HEADER, localReplicaId)

        val handler = handlerFactory(remoteReplicaId)

        // TODO: This probably has a race condition! (maybe add a random number to decide on which connection to choose)
        val replicaIdAlreadyHandled = !connectionManager.addPendingConnection(remoteReplicaId, handler)
        if (replicaIdAlreadyHandled) {
          println(s"Closing newly established connection with $remoteReplicaId, already has open connection to replica")
          null
        } else {
          println(s"Handler created for $remoteReplicaId@${req.getRemoteSocketAddress}")
          handler
        }
      }

    }

  def uri: URI = {
    if (server.getURI == null) null
    else URI.create(server.getURI.toString.replace("http", "ws"))
  }

  def stop(): Unit = server.stop()

  def start(): Unit = {

    JettyWebSocketServletContainerInitializer.configure(
      ctxHandler,
      (_, container) => {
        container.addMapping("/", webSocketCreator)
        container.setIdleTimeout(Duration.ZERO)
      }
    )

    server.start()
  }
}
