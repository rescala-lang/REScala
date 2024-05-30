package encrdtlib.sync.p2p

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import encrdtlib.sync.ConnectionManager
import encrdtlib.sync.p2p.P2PConnectionManager.{Message, State}

import java.net.{InetSocketAddress, URI}
import java.util.concurrent.ConcurrentHashMap
import java.util.function
import java.util.function.BiConsumer
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala

class P2PConnectionManager[S](val localReplicaId: String, localStateProvider: () => S, stateReceivedHandler: S => Unit)(
    implicit stateJsonCodec: JsonValueCodec[S]
) extends ConnectionManager[S] {

  private val handlers           = new ConcurrentHashMap[String, CrdtSyncWebSocketHandler[S]]()
  private val pendingConnections = new ConcurrentHashMap[String, CrdtSyncWebSocketHandler[S]]()

  private val crdtSyncWebSocketClient =
    new CrdtSyncWebSocketClient[S](localReplicaId, createHandler)
  crdtSyncWebSocketClient.start()

  private val crdtSyncWebSocketServer =
    new CrdtSyncWebSocketServer[S](localReplicaId, this, createHandler)
  crdtSyncWebSocketServer.start()

  private def broadcast(message: Message): Unit = {
    println(s"Broadcasting $message to ${handlers.asScala.values.toList}")
    handlers.forEach {
      new BiConsumer[String, CrdtSyncWebSocketHandler[S]] {
        override def accept(t: String, handler: CrdtSyncWebSocketHandler[S]): Unit =
          handler.sendMessage(message)
      }
    }
  }

  def stateChanged(newState: S): Unit = {
    // TODO: execute in network executor
    broadcast(State(writeToString(newState)))
  }

  override def connectToReplica(remoteReplicaId: String, uri: URI): Unit = {
    addPendingConnection(remoteReplicaId, crdtSyncWebSocketClient.connect(remoteReplicaId, uri))
    ()
  }

  def connectToNewPeers(peers: Map[String, String]): Unit = {
    peers.filter { case (rId, rAddrUri) =>
      // don't connect to invalid replica (shouldn't happen)
      if rAddrUri == null then {
        println(s"Received no URI for replica $rId")
        false
      } else
        rId != localReplicaId // don't connect to local replica,
    }.foreach { case (rId, uriString) =>
      try {
        val uri = URI.create(uriString)
        connectToReplica(rId, uri)
      } catch {
        case _: IllegalArgumentException =>
      }
    }
  }

  def addPendingConnection(remoteReplicaId: String, handler: CrdtSyncWebSocketHandler[S]): Boolean = {
    if !handlers.contains(remoteReplicaId) then
      handler == pendingConnections.computeIfAbsent(
        remoteReplicaId,
        new function.Function[Any, CrdtSyncWebSocketHandler[S]] {
          override def apply(t: Any): CrdtSyncWebSocketHandler[S] = handler
        }
      )
    else false
  }

  def promoteHandler(handler: CrdtSyncWebSocketHandler[S]): Boolean = {
    val remoteReplicaId = handler.remoteReplicaId
    if pendingConnections.get(remoteReplicaId) == handler then {
      if
        handler == handlers.computeIfAbsent(
          remoteReplicaId,
          new function.Function[Any, CrdtSyncWebSocketHandler[S]] {
            override def apply(t: Any): CrdtSyncWebSocketHandler[S] = handler
          }
        )
      then {
        if pendingConnections.remove(remoteReplicaId, handler) then {
          println(s"Promoted handler for $remoteReplicaId")
          return true
        }
      }
    }
    return false
  }

  private def createHandler(remoteReplicaId: String): CrdtSyncWebSocketHandler[S] = new CrdtSyncWebSocketHandler[S](
    localReplicaId,
    remoteReplicaId,
    this,
    stateReceivedHandler,
    localStateProvider
  )

  def removeHandler(handler: CrdtSyncWebSocketHandler[S]): Boolean = {
    var removed = false
    if pendingConnections.remove(handler.remoteReplicaId, handler) then {
      println(s"Removing pending handler for ${handler.remoteReplicaId}")
      removed = true
    }
    if handlers.remove(handler.remoteReplicaId, handler) then {
      println(s"Removing handler for ${handler.remoteReplicaId}")
      removed = true
    }
    removed
  }

  def peers: Map[String, String] = handlers.asScala.toMap.map { case (rId, handler) =>
    rId -> {
      val address = handler.getSession.getRemoteAddress
      try {
        address match {
          case inetAddress: InetSocketAddress =>
            // Tests if string is valid URI
            URI.create(s"ws://${inetAddress.getHostName}:${inetAddress.getPort}/").toString
          case _ =>
            println(s"Cannot create uri from non-InetSocketAddress SocketAddress: $address")
            null
        }
      } catch {
        case _: IllegalArgumentException =>
          println(s"Received invalid peer address for $rId: $address")
          null
      }
    }
  }.filterNot { case (_, uri) => uri == null }

  override def remoteAddresses: Set[String] = peers.map { case (rId, uri) => s"$rId@$uri" }.toSet

  def stop(): Unit = {
    println("Stopping ConnectionManager")
    handlers.forEach(new BiConsumer[Any, CrdtSyncWebSocketHandler[S]] {
      override def accept(t: Any, handler: CrdtSyncWebSocketHandler[S]): Unit = handler.close()
    })
    crdtSyncWebSocketClient.stop()
    crdtSyncWebSocketServer.stop()
  }

  def uri: URI = crdtSyncWebSocketServer.uri
}

object P2PConnectionManager {
  val REPLICAID_HEADER = "X-REPLICAID"

  implicit val messageCodec: JsonValueCodec[Message] = JsonCodecMaker.make

  sealed trait Message

  case object RequestPeers extends Message

  case class Peers(peers: Map[String, String]) extends Message

  case object RequestState extends Message

  // TODO: Replace state string with State type (maybe?)
  case class State(state: String) extends Message
}
