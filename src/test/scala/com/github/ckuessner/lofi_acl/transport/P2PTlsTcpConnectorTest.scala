package com.github.ckuessner.lofi_acl.transport

import com.github.ckuessner.lofi_acl.crypto.{IdentityFactory, PrivateIdentity, X509TestHelper}
import munit.FunSuite

import java.util.concurrent.Executors
import javax.net.ssl.SSLHandshakeException
import scala.concurrent.ExecutionContext

class P2PTlsTcpConnectorTest extends FunSuite {
  private val executor       = Executors.newVirtualThreadPerTaskExecutor()
  given ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  test("establishing a connection returns the identity of the peer") {
    val id1: PrivateIdentity = IdentityFactory.createNewIdentity
    val id2: PrivateIdentity = IdentityFactory.createNewIdentity

    val connector1 = P2PTlsTcpConnector(id1)
    val connector2 = P2PTlsTcpConnector(id2)

    val conn1Future = connector1.acceptConnection
    val conn2Future = connector2.connect("localhost", connector1.listenPort)

    for
      (serverSocket, returnedIdFromConn1) <- conn1Future
      (clientSocket, returnedIdFromConn2) <- conn2Future
    yield
      assertEquals(returnedIdFromConn1, id2.getPublic)
      assertEquals(returnedIdFromConn2, id1.getPublic)
      serverSocket.close()
      clientSocket.close()
      connector1.closeServerSocket()
      connector2.closeServerSocket()
  }

  test("establishing a connection fails when using invalid client cert") {
    val serverId: PrivateIdentity = IdentityFactory.createNewIdentity
    var clientId: PrivateIdentity = IdentityFactory.createNewIdentity

    // Use invalid cert
    val clientCert = X509TestHelper.genCertSignedByWrongKey(clientId.identityKey, clientId.tlsKey)
    clientId = clientId.copy(certificateHolder = clientCert)

    val serverConnector = P2PTlsTcpConnector(serverId)
    val clientConnector = P2PTlsTcpConnector(clientId)

    val serverConnFuture = serverConnector.acceptConnection
    val clientConnFuture = clientConnector.connect("localhost", serverConnector.listenPort)

    for
      serverErr         <- serverConnFuture.failed
      (clientSocket, _) <- clientConnFuture // Client can connect, though the socket should be closed by server
    yield
      assert(serverErr.isInstanceOf[SSLHandshakeException])
      clientSocket.close()
      clientConnector.closeServerSocket()
      serverConnector.closeServerSocket()
  }

  test("establishing a connection fails when using invalid server cert") {
    var serverId: PrivateIdentity = IdentityFactory.createNewIdentity
    val clientId: PrivateIdentity = IdentityFactory.createNewIdentity

    val serverCert = X509TestHelper.genCertSignedByWrongKey(serverId.identityKey, serverId.tlsKey)
    serverId = serverId.copy(certificateHolder = serverCert)

    val serverConnector = P2PTlsTcpConnector(serverId)
    val clientConnector = P2PTlsTcpConnector(clientId)

    val serverConnFuture = serverConnector.acceptConnection
    val clientConnFuture = clientConnector.connect("localhost", serverConnector.listenPort)

    for {
      clientErr <- clientConnFuture.failed
      serverErr <- serverConnFuture.failed
    } yield
      assert(clientErr.isInstanceOf[SSLHandshakeException])
      assert(serverErr.isInstanceOf[SSLHandshakeException])
      clientConnector.closeServerSocket()
      serverConnector.closeServerSocket()
  }

}
