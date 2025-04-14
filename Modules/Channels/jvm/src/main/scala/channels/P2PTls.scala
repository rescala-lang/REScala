package channels

import channels.*
import channels.tls.{P2PX509TrustManager, PrivateIdentity, X509Util}
import crypto.PublicIdentity
import de.rmgk.delay.{Async, Sync}
import nl.altindag.ssl.SSLFactory
import nl.altindag.ssl.pem.util.PemUtils
import rdts.base.Uid

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream, IOException}
import java.net.{InetSocketAddress, StandardSocketOptions}
import java.security.cert.X509Certificate
import javax.net.ssl.{SSLServerSocket, SSLSocket}
import scala.concurrent.ExecutionContext

class P2PTls(private val identity: PrivateIdentity) {
  private val sslFactory = {
    val keyManager = {
      val certPemFile = new ByteArrayInputStream(identity.tlsCertPem.getBytes)
      val keyPemFile  = new ByteArrayInputStream(identity.tlsKeyPem.getBytes)
      PemUtils.loadIdentityMaterial(certPemFile, keyPemFile)
    }
    val trustManager = new P2PX509TrustManager()

    SSLFactory
      .builder()
      .withCiphers("TLS_CHACHA20_POLY1305_SHA256")
      .withProtocols("TLSv1.3")
      .withIdentityMaterial(keyManager)
      .withTrustMaterial(trustManager)
      .withNeedClientAuthentication()
      .build()
  }

  def latentListener(listenPort: Int, ec: ExecutionContext): P2PTlsListener = new P2PTlsListener(listenPort, ec)

  def latentConnect(host: String, port: Int, ec: ExecutionContext): LatentConnection[MessageBuffer] =
    (receiver: Receive[MessageBuffer]) =>
      Async[Abort] {
        val socket = sslFactory.getSslSocketFactory
          .createSocket(host, port)
          .asInstanceOf[SSLSocket]
        val remotePublicId = startHandshake(socket).bind
        val conn           = P2PTlsConnection(socket, Uid(remotePublicId.id), receiver)
        ec.execute(() => conn.receiveLoopBlocking())
        conn
      }

  private def startHandshake(socket: SSLSocket): Async[Abort, PublicIdentity] =
    Async.fromCallback {
      socket.addHandshakeCompletedListener { ev =>
        val peerIdentity = X509Util.certificateToPublicIdentity(
          ev.getPeerCertificates()(0).asInstanceOf[X509Certificate]
        )

        Async.handler.succeed(peerIdentity)
      }

      try {
        socket.startHandshake()
      } catch {
        case e: IOException => Async.handler.fail(e)
      }
    }

  class P2PTlsListener private[P2PTls] (_listenPort: Int, executionContext: ExecutionContext)
      extends LatentConnection[MessageBuffer] {
    require(_listenPort >= 0 && _listenPort <= 0xffff)

    private lazy val serverSocket: SSLServerSocket = sslFactory.getSslServerSocketFactory
      .createServerSocket(_listenPort)
      .asInstanceOf[SSLServerSocket]

    /** Bind socket if not already bound and query local port.
      * @return The bound local port
      */
    def listenPort: Int = serverSocket.getLocalPort

    override def prepare(receiver: Receive[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] =
      Async.fromCallback { abort ?=>
        try
          serverSocket // binds port if required

          executionContext.execute(() =>
            while !abort.closeRequest do {
              val socket = serverSocket.accept().asInstanceOf[SSLSocket]
              if socket != null
              then
                startHandshake(socket).map { abort ?=> identity =>
                  val conn = P2PTlsConnection(socket, Uid(identity.id), receiver)
                  executionContext.execute(() => conn.receiveLoopBlocking())
                  conn
                }.run(Async.handler)
            }
          )
        catch {
          case ioException: IOException =>
            Async.handler.fail(ioException)
        }
      }
  }

  private class P2PTlsConnection(
      private val socket: SSLSocket,
      peerReplicaId: Uid, // TODO: change type to PublicIdentity
      receiver: Receive[MessageBuffer]
  ) extends Connection[MessageBuffer] {
    private val outputStream                             = DataOutputStream(socket.getOutputStream)
    private val inputStream                              = DataInputStream(socket.getInputStream)
    private val receivedMessageCallback                  = receiver.messageHandler(this)
    override val authenticatedPeerReplicaId: Option[Uid] = Some(peerReplicaId)

    try
      socket.setOption(StandardSocketOptions.TCP_NODELAY, true)
    catch
      case _: UnsupportedOperationException =>
        println(s"TCP nodelay not supported on this socket")

    override def info: ConnectionInfo =
      socket.getLocalSocketAddress match
        case isa: InetSocketAddress => ConnectionInfo(Option(isa.getHostName), Option(isa.getPort))
        case _                      => ConnectionInfo(None, None)

    override def send(message: MessageBuffer): Async[Any, Unit] = Sync {
      outputStream.synchronized {
        val bytes         = message.asArray
        val messageLength = bytes.length
        outputStream.writeInt(bytes.length)
        outputStream.write(bytes)
      }
    }

    private[P2PTls] def receiveLoopBlocking(): Unit = {
      inputStream.synchronized {
        while true do
          val len   = inputStream.readInt()
          val bytes = inputStream.readNBytes(len)
          receivedMessageCallback.succeed(ArrayMessageBuffer(bytes))
      }
    }

    override def close(): Unit =
      socket.close()
  }
}
