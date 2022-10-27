package com.github.ckuessner.encrdt.sync.client_server

import com.github.ckuessner.encrdt.encrypted.statebased.{DecryptedState, EncryptedState, Replica, TrustedReplica, UntrustedReplica}
import com.github.ckuessner.encrdt.sync.ConnectionManager
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString, writeToString}
import com.google.crypto.tink.Aead
import com.typesafe.scalalogging.Logger
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.websocket.api.{Session, WebSocketAdapter}
import org.eclipse.jetty.websocket.client.WebSocketClient
import org.eclipse.jetty.websocket.server.config.JettyWebSocketServletContainerInitializer
import org.eclipse.jetty.websocket.server.{JettyServerUpgradeRequest, JettyServerUpgradeResponse}

import java.net.URI
import java.time.Duration

trait WebSocketReplica extends Replica {
  protected var server: Server = _
  protected var replicas: Set[Session] = Set.empty

  private val LOG = Logger(getClass)

  override protected def disseminate(encryptedState: EncryptedState): Unit = {
    val serialized = writeToString(encryptedState)
    replicas = replicas.filter(sess => sess.isOpen)
    replicas.foreach(session => session.getRemote.sendString(serialized))
    LOG.debug(s"Disseminating ${encryptedState.versionVector}")
  }

  protected def newReplicaAdded(session: Session): Unit

  protected class ReplicaWsAdapter extends WebSocketAdapter {
    override def onWebSocketConnect(sess: Session): Unit = {
      super.onWebSocketConnect(sess)
      newReplicaAdded(sess)
    }

    override def onWebSocketText(message: String): Unit = {
      super.onWebSocketText(message)
      val encryptedState = readFromString[EncryptedState](message)
      receive(encryptedState)
    }

    override def onWebSocketError(cause: Throwable): Unit = {
      super.onWebSocketError(cause)
      LOG.error(cause.toString)
    }

    override def onWebSocketClose(statusCode: Int, reason: String): Unit = {
      LOG.warn(s"websocket ${super.getSession.getRemoteAddress} closed (removing handler) with $statusCode - $reason")
    }
  }

  object ReplicaWsAdapter {
    def apply(): ReplicaWsAdapter = new ReplicaWsAdapter()

    def apply(req: JettyServerUpgradeRequest, res: JettyServerUpgradeResponse): ReplicaWsAdapter =
      ReplicaWsAdapter.apply()
  }

  def start(): Unit = {
    if (server != null) throw new IllegalStateException("Server already running")
    server = new Server()
    val connector = new ServerConnector(server)
    server.addConnector(connector)
    val ctxHandler = new ServletContextHandler()
    ctxHandler.setContextPath("/")
    server.setHandler(ctxHandler)

    JettyWebSocketServletContainerInitializer.configure(ctxHandler, (ctx, container) => {
      container.addMapping("/", (r,s) => ReplicaWsAdapter.apply(r,s))
      container.setIdleTimeout(Duration.ZERO)
      ctx.setSessionTimeout(0)
      container.setMaxBinaryMessageSize(Long.MaxValue)
      container.setMaxTextMessageSize(Long.MaxValue)
    })

    server.start()

    LOG.debug("Server started")
  }

  def stop(): Unit = {
    if (server == null) return
    replicas.foreach(session => session.disconnect())
    server.stop()
    LOG.debug("Server stopped")
  }

  def uri: URI = {
    if (server == null || server.getURI == null) null
    else URI.create(server.getURI.toString.replace("http", "ws"))
  }

  def remoteAddresses: Set[String] = replicas.map(sess => sess.getRemoteAddress.toString)
}

class UntrustedReplicaWebSocketServer extends UntrustedReplica(Set.empty) with WebSocketReplica {
  private val LOG = Logger(getClass)

  // Setup of WS Server
  override protected def newReplicaAdded(session: Session): Unit = {
    replicas = replicas + session
    stateStore.foreach(state => {
      session.getRemote.sendString(writeToString(state))
    })
    LOG.info(s"New connection: ${session.getRemote.getRemoteAddress}")
  }
}

abstract class TrustedReplicaWebSocketClient[T](replicaId: String, aead: Aead)
                                               (implicit val jsonValueCodec: JsonValueCodec[T])
  extends TrustedReplica[T](replicaId, aead) with WebSocketReplica with ConnectionManager[T] {

  private val LOG = Logger(getClass)

  private val webSocketClient: WebSocketClient = new WebSocketClient()
  webSocketClient.setIdleTimeout(Duration.ZERO) // Infinite timeout
  webSocketClient.start()
  webSocketClient.setMaxBinaryMessageSize(Long.MaxValue)
  webSocketClient.setMaxTextMessageSize(Long.MaxValue)

  override protected def newReplicaAdded(session: Session): Unit = {
    replicas = replicas + session
    val encryptedState = DecryptedState(localState(), versionVector).encrypt(aead)
    session.getRemote.sendString(writeToString(encryptedState))
    LOG.info(s"Connected to ${session.getRemote.getRemoteAddress}")
  }

  def connectToReplica(remoteReplicaId: String, uri: URI): Unit = {
    webSocketClient.connect(new ReplicaWsAdapter(), uri)
    LOG.info(s"Connecting to $uri")
  }

  override def stop(): Unit = {
    super.stop()
    webSocketClient.stop()
  }
}
