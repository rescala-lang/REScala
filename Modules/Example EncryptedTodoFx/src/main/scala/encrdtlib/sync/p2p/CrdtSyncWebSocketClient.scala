package encrdtlib.sync.p2p

import encrdtlib.sync.p2p.P2PConnectionManager.REPLICAID_HEADER
import org.eclipse.jetty.websocket.client.{ClientUpgradeRequest, WebSocketClient}

import java.net.URI
import java.time.Duration

class CrdtSyncWebSocketClient[S](
    val localReplicaId: String,
    private val handlerFactory: String => CrdtSyncWebSocketHandler[S]
) {

  private val webSocketClient: WebSocketClient = new WebSocketClient()
  webSocketClient.setIdleTimeout(Duration.ZERO) // Infinite timeout

  def connect(remoteReplicaId: String, uri: URI): CrdtSyncWebSocketHandler[S] = {
    println(s"Connecting to $remoteReplicaId@$uri")
    val clientUpgradeRequest = new ClientUpgradeRequest()
    clientUpgradeRequest.setHeader(REPLICAID_HEADER, localReplicaId)
    val handler = handlerFactory(remoteReplicaId)
    webSocketClient.connect(handler, uri, clientUpgradeRequest)
    handler
  }

  def start(): Unit = webSocketClient.start()

  def stop(): Unit = webSocketClient.stop()
}
