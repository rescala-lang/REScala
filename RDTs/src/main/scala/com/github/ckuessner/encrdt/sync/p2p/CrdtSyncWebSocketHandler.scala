package com.github.ckuessner.encrdt.sync.p2p

import com.github.ckuessner.encrdt.sync.p2p.P2PConnectionManager.{Message, RequestPeers, RequestState}
import CrdtSyncWebSocketHandler.{clientShutdownCloseStatus, duplicateCloseStatus}
import P2PConnectionManager._
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString, writeToString}
import com.typesafe.scalalogging.Logger
import org.eclipse.jetty.websocket.api.exceptions.WebSocketTimeoutException
import org.eclipse.jetty.websocket.api.{CloseStatus, Session, WebSocketAdapter, WebSocketBehavior}

import scala.util.{Failure, Success, Try}

class CrdtSyncWebSocketHandler[S](val localReplicaId: String,
                                  val remoteReplicaId: String,
                                  private val connectionManager: P2PConnectionManager[S],
                                  private val stateReceivedHandler: S => Unit,
                                  private val localStateProvider: () => S)
                                 (implicit valueCodec: JsonValueCodec[S]) extends WebSocketAdapter {

  private val LOG = Logger(getClass)

  override def onWebSocketConnect(sess: Session): Unit = {
    if (sess.getBehavior == WebSocketBehavior.CLIENT) {
      val rIdFromHeader = sess.getUpgradeResponse.getHeader(REPLICAID_HEADER)
      if (rIdFromHeader != remoteReplicaId) {
        LOG.warn(s"Closing connection to ${sess.getRemoteAddress}, because replicaId doesn't match ${rIdFromHeader} != ${remoteReplicaId}")

        connectionManager.removeHandler(this)
        sess.close(new CloseStatus(2500, "replicaId mismatch"))
        return
      }
    }

    super.onWebSocketConnect(sess)

    if (!connectionManager.promoteHandler(this)) {
      LOG.debug(s"Can't promote handler for $remoteReplicaId")
      connectionManager.removeHandler(this)
      close(duplicateCloseStatus)
      return
    }

    LOG.info(s"Connection with $remoteReplicaId @ ${sess.getRemoteAddress} is now open")

    // Requesting remote peers, connecting to them on receive of Peers(...)
    sendMessage(RequestPeers)
    sendMessage(RequestState) // TODO: pass VectorClock to tell whether update needs to be transmitted?
  }

  override def onWebSocketText(messageString: String): Unit = {
    Try {
      readFromString[Message](messageString)
    } match {
      case Failure(exception) =>
        LOG.warn(s"failed to parse msg from $remoteReplicaId: $messageString \nBecause:\n $exception")
      case Success(message) =>
        LOG.debug(s"received from $remoteReplicaId: $message")
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
    LOG.warn(s"websocket to $remoteReplicaId@${getRemote.getRemoteAddress} closed (removing handler) with $statusCode - $reason")
    connectionManager.removeHandler(this)
  }

  override def onWebSocketError(cause: Throwable): Unit = {
    if (cause.isInstanceOf[WebSocketTimeoutException]) {
      LOG.warn(s"Connection to $remoteReplicaId timed out")
    } else {
      LOG.error(s"websocket received error (removing handler): $cause")
    }
    connectionManager.removeHandler(this)
  }

  def close(): Unit = {
    close(clientShutdownCloseStatus)
  }

  def close(closeStatus: CloseStatus): Unit = {
    if (getSession == null) LOG.warn(s"session with $remoteReplicaId already closed, not closing")
    else getSession.close(closeStatus)
  }

  def sendMessage(message: Message): Unit = {
    if (isConnected) {
      getSession.getRemote.sendString(writeToString(message))
      LOG.debug(s"Sending to $remoteReplicaId: $message")
    } else {
      LOG.warn(s"Not sending message to $remoteReplicaId, session is closed: $message")
    }
  }
}

object CrdtSyncWebSocketHandler {
  val clientShutdownCloseStatus = new CloseStatus(4000, "ClientShutdown")
  val duplicateCloseStatus = new CloseStatus(4001, "Already has open connection")
  val replicaIdNotSpecifiedCloseStatus = new CloseStatus(4002, "Did not specify replicaId")
}
