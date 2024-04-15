package lofi_acl.crypto

import org.bouncycastle.cert.X509CertificateHolder

object IdentityFactory {
  def createNewIdentity: PrivateIdentity = {
    val identityKey                       = Ed25519Util.generateNewKeyPair
    val tlsKey                            = Ed25519Util.generateNewKeyPair
    val certHolder: X509CertificateHolder = X509Util.generateCustomP2PX509Certificate(identityKey, tlsKey)

    PrivateIdentity(identityKey, tlsKey, certHolder)
  }
}
