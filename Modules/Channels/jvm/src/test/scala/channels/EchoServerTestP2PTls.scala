package channels

import channels.EchoServerTestP2PTls.{p2pTls1, p2pTls2}
import channels.tls.{IdentityFactory, PrivateIdentity}

class EchoServerTestP2PTls extends EchoCommunicationTest(
      ec => {
        val latentConnection = p2pTls1.latentListener(0, ec)
        (latentConnection.listenPort, latentConnection)
      },
      ec => port => p2pTls2.latentConnect("localhost", port, ec)
    )

object EchoServerTestP2PTls {
  val id1: PrivateIdentity = IdentityFactory.createNewIdentity
  val id2: PrivateIdentity = IdentityFactory.createNewIdentity
  val p2pTls1              = P2PTls(id1)
  val p2pTls2              = P2PTls(id2)
}
