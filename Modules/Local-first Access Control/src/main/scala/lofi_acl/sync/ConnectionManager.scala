package lofi_acl.sync

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReaderException, JsonValueCodec, readFromArray, writeToArray}
import lofi_acl.crypto.{PrivateIdentity, PublicIdentity}
import lofi_acl.transport.P2PTlsTcpConnector

import java.io.{DataInputStream, DataOutputStream, IOException}
import java.util
import java.util.concurrent.Executors
import javax.net.ssl.SSLSocket
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class ConnectionManager[MSG](
    privateIdentity: PrivateIdentity,
    messageHandler: MessageHandler[MSG]
)(using msgCodec: JsonValueCodec[MSG]) {
  private val executor               = Executors.newCachedThreadPool()
  private given ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  private val connector = P2PTlsTcpConnector(privateIdentity)

  @volatile private var running: Boolean           = false
  @volatile private var stopped: Boolean           = false
  private var listenerFuture: Option[Future[Unit]] = None

  @volatile private var connections: Map[PublicIdentity, SSLSocket] = Map.empty
  private val localPublicId                                         = privateIdentity.getPublic

  private val receiverFutureLock: Object                                     = Object()
  private var receiverThreads: Set[util.concurrent.Future[?]]                = Set.empty
  @volatile private var outputStreams: Map[PublicIdentity, DataOutputStream] = Map.empty

  /** Sends a message to the user and returns true, if a connections exists. Otherwise discards message and returns false.
    *
    * If the ConnectionManager is shut down, this method also returns false.
    *
    * @param user The user to send the message to.
    * @param msg The message to send.
    * @return true if a connections exists, otherwise false.
    */
  def send(user: PublicIdentity, msg: MSG): Boolean = {
    if stopped then return false
    // TODO: Check if read from volatile outputStreams is sufficient
    outputStreams.get(user) match
      case Some(outputStream) =>
        Future {
          val encodedMsg = writeToArray(msg)
          outputStream.synchronized {
            outputStream.writeInt(encodedMsg.length)
            outputStream.write(encodedMsg)
            // TODO: Flush?
          }
        }
        true
      case None => false
  }

  def acceptIncomingConnections(): Unit = {
    println(s"$localPublicId is accepting connections on port ${connector.listenPort}")

    def acceptConnection(): Unit =
      if stopped then running = false
      else
        val connectionFuture = connector.acceptConnection
        connectionFuture.onComplete {
          case Failure(exception) =>
            exception.printStackTrace()
            running = false
            sys.error("Stopping listener")
          case Success((socket, peerIdentity)) =>
            connectionEstablished(socket, peerIdentity, establishedByRemote = true)
            acceptConnection()
        }

    this.synchronized {
      if running then throw IllegalStateException("Already listening")
      if stopped then throw UnsupportedOperationException("Cannot restart listener of Sync")
      running = true
      acceptConnection()
    }
  }

  def listenPort: Option[Int] =
    if running then Some(connector.listenPort)
    else None

  def shutdown(): Unit = {
    stopped = true
    connector.closeServerSocket()
    this.synchronized {
      outputStreams = Map.empty
      connections.foreach(_._2.close())
    }
    receiverFutureLock.synchronized {
      receiverThreads.foreach(_.cancel(true))
    }
    val _ = executor.shutdownNow()
  }

  def connectTo(host: String, port: Int): Unit = {
    connector.connect(host, port).onComplete {
      case Failure(exception)        => exception.printStackTrace()
      case Success((socket, peerId)) => connectionEstablished(socket, peerId, establishedByRemote = false)
    }
  }

  def connectToExpectingUserIfNoConnectionExists(host: String, port: Int, expectedUser: PublicIdentity): Unit = {
    this.synchronized {
      if connections.contains(expectedUser) then return
    }

    connector.connect(host, port).onComplete {
      case Failure(exception) => exception.printStackTrace()
      case Success((socket, peerId: PublicIdentity)) =>
        if expectedUser != peerId then connectionEstablished(socket, peerId, establishedByRemote = false)
        else
          sys.error(s"Expecting $expectedUser at $host:$port but connected to $peerId. Closing socket.")
          try {
            socket.close()
          } catch
            case e: IOException => e.printStackTrace()
    }
  }

  private def connectionEstablished(
      socket: SSLSocket,
      peerIdentity: PublicIdentity,
      establishedByRemote: Boolean
  ): Unit = {
    println(s"Established connection with $peerIdentity at ${socket.getRemoteSocketAddress}")

    this.synchronized {
      connections.get(peerIdentity) match
        case Some(existingConnection) =>
          // Avoid duplicate connections while allowing one connection to persist (make sure the same connection is
          // terminated by both sides, i.e., not both).
          // Use the socket initiated by the peer with higher ID
          // TODO: If one side attempts two connections, then this probably won't work reliably
          if establishedByRemote && peerIdentity.id > localPublicId.id
            || !establishedByRemote && peerIdentity.id < localPublicId.id
          then
            try {
              existingConnection.close()
            } catch { case e: IOException => }
            connections = connections.updated(peerIdentity, socket)
            receiveFrom(peerIdentity, socket)
          else
            try {
              socket.close()
            } catch { case e: IOException => }
        case None =>
          connections = connections.updated(peerIdentity, socket)
          receiveFrom(peerIdentity, socket)
    }
  }

  /** Start a thread that receives messages from the socket, parses them and forwards them to the MessageHandler.
    *
    * @param identity The identity of the other side.
    * @param socket The socket to receive messages on.
    */
  private def receiveFrom(identity: PublicIdentity, socket: SSLSocket): Unit = {
    val receiverFuture: util.concurrent.Future[?] = executor.submit(
      new Runnable:
        override def run(): Unit = {
          val input = new DataInputStream(socket.getInputStream)
          while (!stopped)
            try {
              val lengthOfMessage = input.readInt() // Length of message is sent before message itself.
              val message         = input.readNBytes(lengthOfMessage)
              val msg             = readFromArray[MSG](message)
              messageHandler.receivedMessage(msg, identity)
            } catch {
              case e: IOException =>
                try { socket.close() }
                catch { case e: IOException => }
                this.synchronized {
                  // Only remove socket from map, if it hasn't been replaced already
                  connections.get(identity) match
                    case Some(storedSocket) =>
                      if storedSocket eq socket
                      then connections = connections.removed(identity)
                    case None =>
                }
              case jsonEx: JsonReaderException => jsonEx.printStackTrace()
              case e: InterruptedException =>
                try { socket.close() }
                catch { case e: IOException => }
            }
        }
    )
    receiverFutureLock.synchronized {
      receiverThreads = receiverThreads + receiverFuture
    }
  }

}