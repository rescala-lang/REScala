package loci
package registry

import communicator.{Connection, Connector, Listener}
import messaging.{ConnectionsBase, Message}
import transmitter.RemoteRef

import java.util.concurrent.atomic.AtomicLong

import scala.util.{Failure, Success, Try}

object Connections {
  type Protocol = ConnectionsBase.Protocol

  private final case class RemoteRef(
      id: Long, protocol: Protocol)(
      connections: Connections[_]) extends transmitter.RemoteRef {
    val doDisconnected = Notice.Steady[Unit]

    def connected = connections.isConnected(this)
    def disconnect() = connections.disconnect(this)
    val disconnected = doDisconnected.notice

    override def toString: String = s"remote#$id[$protocol]"
  }
}

class Connections[M: Message.Method]
    extends ConnectionsBase[RemoteRef, Message[M]] {

  protected def deserializeMessage(message: MessageBuffer) = {
    val result = Message deserialize message
    result.failed foreach { logging.warn("could not parse message", _) }
    result
  }

  protected def serializeMessage(message: Message[M]) =
    Message serialize message

  protected class State extends BaseState {
    private val counter = new AtomicLong(1)
    def createId() = counter.getAndIncrement()
  }

  protected val state: State = new State

  def connect(
      connector: Connector[Connections.Protocol])(
      handler: Try[RemoteRef] => Unit): Unit =
    connector.connect() { addConnection(_, handler) }

  def listen(listener: Listener[Connections.Protocol])(
      handler: Try[RemoteRef] => Unit): Try[Unit] =
    listener.startListening() { addConnection(_, handler) } match {
      case Success(listening) =>
        addListening(listening)
      case Failure(exception) =>
        Failure(exception)
    }

  private def addConnection(
      connection: Try[Connection[Connections.Protocol]],
      handler: Try[RemoteRef] => Unit): Unit =
    connection match {
      case Success(connection) =>
        val remote = Connections.RemoteRef(
          state.createId(), connection.protocol)(this)
        handler(addConnection(remote, connection) map { _ => remote })

      case Failure(exception) =>
        handler(Failure(exception))
    }

  remoteLeft foreach {
    case remote: Connections.RemoteRef =>
      remote.doDisconnected.set()
    case _ =>
  }
}
