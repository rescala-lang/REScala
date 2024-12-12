package lofi_acl.transport

import lofi_acl.crypto.{PrivateIdentity, PublicIdentity, X509Util}
import nl.altindag.ssl.SSLFactory
import nl.altindag.ssl.pem.util.PemUtils

import java.io.{ByteArrayInputStream, IOException}
import java.security.cert.X509Certificate
import javax.net.ssl.{SSLServerSocket, SSLSocket}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

class P2PTlsTcpConnector(private val identity: PrivateIdentity, _listenPort: Int = 0) {
  require(_listenPort >= 0 && _listenPort <= 0xffff)

  private val certPemFile = new ByteArrayInputStream(identity.tlsCertPem.getBytes)
  private val keyPemFile  = new ByteArrayInputStream(identity.tlsKeyPem.getBytes)

  private val keyManager   = PemUtils.loadIdentityMaterial(certPemFile, keyPemFile)
  private val trustManager = new P2PX509TrustManager()

  def listenPort: Int = sslServerSocket.getLocalPort

  private val sslFactory = SSLFactory
    .builder()
    .withCiphers("TLS_CHACHA20_POLY1305_SHA256")
    .withProtocols("TLSv1.3")
    .withIdentityMaterial(keyManager)
    .withTrustMaterial(trustManager)
    .withNeedClientAuthentication()
    .build()

  private val sslServerSocket = sslFactory.getSslServerSocketFactory
    .createServerSocket(_listenPort)
    .asInstanceOf[SSLServerSocket]

  def acceptConnection(using ec: ExecutionContext): Future[(SSLSocket, PublicIdentity)] =
    for
      socket         <- Future(sslServerSocket.accept().asInstanceOf[SSLSocket])
      publicIdentity <- doHandshake(socket)
    yield (socket, publicIdentity)

  def connect(host: String, port: Int)(using ec: ExecutionContext): Future[(SSLSocket, PublicIdentity)] =
    for
      socket <- Future {
        sslFactory.getSslSocketFactory
          .createSocket(host, port)
          .asInstanceOf[SSLSocket]
      }
      publicIdentity <- doHandshake(socket)
    yield (socket, publicIdentity)

  def closeServerSocket(): Try[Unit] = Try {
    sslServerSocket.close()
  }

  private def doHandshake(socket: SSLSocket)(using ec: ExecutionContext): Future[PublicIdentity] = {
    val identityPromise = Promise[PublicIdentity]

    socket.addHandshakeCompletedListener { ev =>
      val peerIdentity = X509Util.certificateToPublicIdentity(
        ev.getPeerCertificates()(0).asInstanceOf[X509Certificate]
      )

      identityPromise.success(peerIdentity)
    }

    try {
      socket.startHandshake()
    } catch {
      case e: IOException => identityPromise.failure(e)
    }

    identityPromise.future
  }
}
