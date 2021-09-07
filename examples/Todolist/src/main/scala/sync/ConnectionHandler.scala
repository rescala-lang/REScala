package de.ckuessner
package sync

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import io.javalin.Javalin
import io.javalin.websocket.{WsCloseContext, WsConnectContext, WsContext, WsMessageContext}
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.websocket.api.{CloseStatus, Session, WebSocketListener}
import org.eclipse.jetty.websocket.client.{ClientUpgradeRequest, WebSocketClient}

import java.net.URI
import java.util.concurrent.ConcurrentHashMap

class ConnectionHandler[S](val replicaId: String, stateReceivedHandler: S => Unit, localStateProvider: () => S)
                          (implicit valueCodec: JsonValueCodec[S]) {

  import ConnectionHandler._

  private var peerAddresses: Map[String, String] = Map.empty

  private val httpClient = new HttpClient()
  private val webSocketClient = new WebSocketClient(httpClient)

  private val serverPeers: ConcurrentHashMap[String, WsContext] = new ConcurrentHashMap()
  private val clientPeers: ConcurrentHashMap[String, WebSocketClientHandler] = new ConcurrentHashMap()

  // Start Server WebSocket handler
  WebSocketServerHandler.start()
  // Start Client WebSocket handler
  webSocketClient.start()

  def address: String = WebSocketServerHandler.bindAddress

  private def broadcast(message: Message): Unit = {
    Console.println(s"broadcasting $message")
    val messageString = writeToString(message)
    serverPeers.forEach((_, wsContext) => wsContext.send(messageString))
    clientPeers.forEach((_, clientHandler) => clientHandler.sendMessage(message))
  }

  def stateChanged(): Unit = {
    broadcast(State(writeToString[S](localStateProvider())))
  }

  private def handlePeerChange(receivedPeerList: Map[String, String]): Unit = {
    receivedPeerList.filter { case (remoteReplicaId, _) =>
      !serverPeers.contains(remoteReplicaId) && !clientPeers.contains(remoteReplicaId)
    }.foreach { case (rId, rAddress) => connectWithClientWs(rId, rAddress) }
  }

  def connectWithClientWs(remoteReplicaId: String, remoteAddr: String): Unit = {
    Console.println(s"Connecting to $remoteReplicaId@$remoteAddr")
    if (remoteReplicaId == replicaId) return
    val upgradeRequest = new ClientUpgradeRequest()
    upgradeRequest.setHeader(REPLICAID_HEADER, replicaId)
    val handler = new WebSocketClientHandler(remoteReplicaId)
    clientPeers.put(remoteReplicaId, handler)
    webSocketClient.connect(handler, URI.create(remoteAddr), upgradeRequest)
  }

  private object WebSocketServerHandler {
    private var server: Option[Javalin] = None

    def start(): Unit =
      server = Some(
        Javalin.create()
          .ws("/", ws => {
            ws.onConnect(handleConnect)
            ws.onMessage(handleMessage)
            ws.onClose(handleClose)
          }).start(0))

    // TODO: for now localhost, configure jetty to bind to some specified address
    def bindAddress: String = s"ws://localhost:${server.get.port()}/"

    def stop(): Unit = {
      serverPeers.forEach((_, ctx) => ctx.session.close(clientShutdownCloseStatus))
      if (server.isDefined) server.get.stop()
    }

    def handleMessage(ctx: WsMessageContext): Unit = {
      readFromString[Message](ctx.message) match {
        case RequestPeers =>
          ctx.send(writeToString[Message](Peers(peerAddresses)))

        case Peers(remotePeers) =>
          handlePeerChange(remotePeers)

        case RequestState =>
          ctx.send(writeToString(localStateProvider()))

        case State(stateString) =>
          val state: S = readFromString[S](stateString)
          stateReceivedHandler(state)
      }
    }

    def handleConnect(ctx: WsConnectContext): Unit = {
      // TODO: close client peer
      val remoteReplicaId = ctx.header(REPLICAID_HEADER)
      Console.println(s"Incoming connection of $remoteReplicaId from ${ctx.session.getRemoteAddress}")
      serverPeers.put(remoteReplicaId, ctx)
      peerAddresses = peerAddresses + (replicaId -> ctx.session.getRemoteAddress.toString)
      ctx.send(writeToString[Message](State(writeToString[S](localStateProvider()))))
    }

    def handleClose(ctx: WsCloseContext): Unit = {
      val remoteReplicaId = ctx.header(REPLICAID_HEADER)
      Console.println(s"Server WS closing connection to ${ctx.session.getRemoteAddress} $remoteReplicaId with reason: ${ctx.reason()}")
      serverPeers.remove(remoteReplicaId)
    }

  }

  private class WebSocketClientHandler(val remoteReplicaId: String) extends WebSocketListener {
    private var session: Option[Session] = None

    def isOpen: Boolean = session.isDefined && session.get.isOpen

    def close(closeStatus: CloseStatus): Boolean = {
      if (session.isEmpty) false
      else {
        session.get.close(closeStatus)
        true
      }
    }

    override def onWebSocketConnect(session: Session): Unit = {

      // TODO: move to server
      //val replicaIdFromHeader = session.getUpgradeRequest.getHeader(REPLICAID_HEADER)
      //if (remoteReplicaId != replicaIdFromHeader) {
      //  Console.err.println(s"WebSocketClientHandler connected to $replicaIdFromHeader expected $remoteReplicaId")
      //  session.close(2500, "expected different replicaId")
      //  return
      //}

      this.session = Some(session)
      Console.println("Connected to " + session.getRemoteAddress)
    }

    override def onWebSocketText(messageString: String): Unit = {
      readFromString[Message](messageString) match {
        case RequestPeers =>
          sendMessage(Peers(peerAddresses))

        case Peers(peers) =>
          handlePeerChange(peers)

        case RequestState =>
          sendMessage(State(writeToString(localStateProvider())))

        case State(stateString) =>
          val receivedState: S = readFromString[S](stateString)
          stateReceivedHandler(receivedState)
      }
    }

    def sendMessage(message: Message): Unit = {
      if (session.isEmpty) {
        Console.err.println(s"Can't send message $message to $remoteReplicaId, since session is not defined")
        // TODO: handle not-connected
        return
      }
      val messageString = writeToString[Message](message)
      session.get.getRemote.sendString(messageString)
    }

    override def onWebSocketClose(statusCode: Int, reason: String): Unit = {
      Console.println(s"$replicaId disconnected $statusCode - $reason")
      session = None
      clientPeers.remove(remoteReplicaId)
      // Don't reconnect if close status is clientShutdownCloseStatus
    }

    override def onWebSocketError(cause: Throwable): Unit = {
      // TODO: handle error (resend last state?)
      Console.err.println(s"Error in WSCConnection ${session.orNull} received an error ${cause.toString}")
      session = None
      clientPeers.remove(remoteReplicaId)
    }

    override def onWebSocketBinary(payload: Array[Byte], offset: Int, len: Int): Unit = {
      Console.err.println("Received binary payload. Unhandled!")
    }
  }

  def shutdown(): Unit = {
    clientPeers.forEach((_, handler) => handler.close(clientShutdownCloseStatus))
    webSocketClient.stop()
    WebSocketServerHandler.stop()
  }

}

object ConnectionHandler {
  private val REPLICAID_HEADER = "X-REPLICAID"

  private val clientShutdownCloseStatus = new CloseStatus(2000, "ClientShutdown")

  private implicit val messageCodec: JsonValueCodec[Message] = JsonCodecMaker.make

  sealed trait Message

  case object RequestPeers extends Message

  case class Peers(peers: Map[String, String]) extends Message

  case object RequestState extends Message

  case class State(state: String) extends Message
}