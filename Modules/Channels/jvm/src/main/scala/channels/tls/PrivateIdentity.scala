package channels.tls

import channels.tls.X509Util.toPem
import crypto.{CertificatePem, Ed25519Util, PrivateKeyPem, PublicIdentity}
import org.bouncycastle.cert.X509CertificateHolder

import java.security.KeyPair

// TODO: Rename
case class PrivateIdentity(identityKey: KeyPair, tlsKey: KeyPair, certificateHolder: X509CertificateHolder) {

  def tlsCertPem: CertificatePem = certificateHolder.toPem

  def tlsKeyPem: PrivateKeyPem = Ed25519Util.privateKeyToPem(tlsKey.getPrivate)

  def getPublic: PublicIdentity = PublicIdentity(
    Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(identityKey.getPublic)
  )
}
