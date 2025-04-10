package channels.tls

import crypto.Ed25519Util
import org.bouncycastle.cert.X509CertificateHolder

import java.security.KeyPair

object IdentityFactory {
  def createNewIdentity: PrivateIdentity = {
    val identityKey = Ed25519Util.generateNewKeyPair
    fromIdentityKey(identityKey)
  }

  def fromIdentityKey(identityKey: KeyPair): PrivateIdentity = {
    val tlsKey                            = Ed25519Util.generateNewKeyPair
    val certHolder: X509CertificateHolder = X509Util.generateCustomP2PX509Certificate(identityKey, tlsKey)
    PrivateIdentity(identityKey, tlsKey, certHolder)
  }
}
