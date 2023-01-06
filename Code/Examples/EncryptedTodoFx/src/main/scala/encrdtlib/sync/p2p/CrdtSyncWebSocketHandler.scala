package encrdtlib.sync.p2p

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString, writeToString}
import org.eclipse.jetty.websocket.api.exceptions.WebSocketTimeoutException
import org.eclipse.jetty.websocket.api.{CloseStatus, Session, WebSocketAdapter, WebSocketBehavior}
import CrdtSyncWebSocketHandler.{clientShutdownCloseStatus, duplicateCloseStatus}
import P2PConnectionManager.{
  Message, Peers, REPLICAID_HEADER, RequestPeers, RequestState, State
}

import scala.util.{Failure, Success, Try}

class CrdtSyncWebSocketHandler[S](
    val localReplicaId: String,
    val remoteReplicaId: String,
    private val connectionManager: P2PConnectionManager[S],
    private val stateReceivedHandler: S => Unit,
    private val localStateProvider: () => S
)(implicit valueCodec: JsonValueCodec[S]) extends WebSocketAdapter {

  override def onWebSocketConnect(sess: Session): Unit = {
    if (sess.getBehavior == WebSocketBehavior.CLIENT) {
      val rIdFromHeader = sess.getUpgradeResponse.getHeader(REPLICAID_HEADER)
      if (rIdFromHeader != remoteReplicaId) {
        println(
          s"Closing connection to ${sess.getRemoteAddress}, because replicaId doesn't match ${rIdFromHeader} != ${remoteReplicaId}"
        )

        connectionManager.removeHandler(this)
        sess.close(new CloseStatus(2500, "replicaId mismatch"))
        return
      }
    }

    super.onWebSocketConnect(sess)

    if (!connectionManager.promoteHandler(this)) {
      println(s"Can't promote handler for $remoteReplicaId")
      connectionManager.removeHandler(this)
      close(duplicateCloseStatus)
      return
    }

    println(s"Connection with $remoteReplicaId @ ${sess.getRemoteAddress} is now open")

    // Requesting remote peers, connecting to them on receive of Peers(...)
    sendMessage(RequestPeers)
    sendMessage(RequestState) // TODO: pass VectorClock to tell whether update needs to be transmitted?
  }

  override def onWebSocketText(messageString: String): Unit = {
    Try {
      readFromString[Message](messageString)
    } match {
      case Failure(exception) =>
        println(s"failed to parse msg from $remoteReplicaId: $messageString \nBecause:\n $exception")
      case Success(message) =>
        println(s"received from $remoteReplicaId: $message")
        handleMessage(message)
    }
  }

  private def handleMessage(message: Message): Unit = message match {
    case RequestPeers =>
      sendMessage(Peers(connectionManager.peers))

    case Peers(peers) =>
      connectionManager.connectToNewPeers(peers)

    case RequestState =>
      sendMessage(State(writeToString(localStateProvider())))

    case State(state) =>
      stateReceivedHandler(readFromString[S](state))
  }

  override def onWebSocketClose(statusCode: Int, reason: String): Unit = {
    println(
      s"websocket to $remoteReplicaId@${getRemote.getRemoteAddress} closed (removing handler) with $statusCode - $reason"
    )
    connectionManager.removeHandler(this)
    ()
  }

  override def onWebSocketError(cause: Throwable): Unit = {
    if (cause.isInstanceOf[WebSocketTimeoutException]) {
      println(s"Connection to $remoteReplicaId timed out")
    } else {
      println(s"websocket received error (removing handler): $cause")
    }
    connectionManager.removeHandler(this)
    ()
  }

  def close(): Unit = {
    close(clientShutdownCloseStatus)
  }

  def close(closeStatus: CloseStatus): Unit = {
    if (getSession == null) println(s"session with $remoteReplicaId already closed, not closing")
    else getSession.close(closeStatus)
  }

  def sendMessage(message: Message): Unit = {
    if (isConnected) {
      getSession.getRemote.sendString(writeToString(message))
      println(s"Sending to $remoteReplicaId: $message")
    } else {
      println(s"Not sending message to $remoteReplicaId, session is closed: $message")
    }
  }
}

object CrdtSyncWebSocketHandler {
  val clientShutdownCloseStatus        = new CloseStatus(4000, "ClientShutdown")
  val duplicateCloseStatus             = new CloseStatus(4001, "Already has open connection")
  val replicaIdNotSpecifiedCloseStatus = new CloseStatus(4002, "Did not specify replicaId")
}
