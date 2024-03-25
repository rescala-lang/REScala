package com.github.ckuessner.lofi_acl.transport

import com.github.ckuessner.lofi_acl.crypto.{PrivateIdentity, PublicIdentity, X509Util}
import nl.altindag.ssl.SSLFactory
import nl.altindag.ssl.pem.util.PemUtils

import java.io.ByteArrayInputStream
import java.security.cert.X509Certificate
import javax.net.ssl.{SSLServerSocket, SSLSocket}
import scala.concurrent.{ExecutionContext, Future, Promise}

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

  def acceptConnection(implicit ec: ExecutionContext): Future[(SSLSocket, PublicIdentity)] = for {
    socket         <- Future(sslServerSocket.accept().asInstanceOf[SSLSocket])
    publicIdentity <- doHandshake(socket)
  } yield (socket, publicIdentity)

  def connect(host: String, port: Int)(implicit ec: ExecutionContext): Future[(SSLSocket, PublicIdentity)] = for {
    socket <- Future {
      sslFactory.getSslSocketFactory
        .createSocket(host, port)
        .asInstanceOf[SSLSocket]
    }
    publicIdentity <- doHandshake(socket)
  } yield (socket, publicIdentity)

  def closeServerSocket(): Unit = {
    sslServerSocket.close()
  }

  private def doHandshake(socket: SSLSocket): Future[PublicIdentity] = {
    val identityPromise = Promise[PublicIdentity]

    socket.addHandshakeCompletedListener { ev =>
      val peerIdentity = X509Util.certificateToPublicIdentity(
        ev.getPeerCertificates()(0).asInstanceOf[X509Certificate]
      )

      identityPromise.success(peerIdentity)
    }

    socket.startHandshake()

    identityPromise.future
  }
}
