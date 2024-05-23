package lofi_acl.crypto

import lofi_acl.crypto.X509Util.toPem
import org.bouncycastle.cert.X509CertificateHolder
import rdts.base.Uid

import java.nio.charset.StandardCharsets
import java.security.{KeyPair, PublicKey}

case class PrivateIdentity(identityKey: KeyPair, tlsKey: KeyPair, certificateHolder: X509CertificateHolder) {

  def tlsCertPem: CertificatePem = certificateHolder.toPem

  def tlsKeyPem: PrivateKeyPem = Ed25519Util.privateKeyToPem(tlsKey.getPrivate)

  def getPublic: PublicIdentity = PublicIdentity(
    Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(identityKey.getPublic)
  )
}

case class PublicIdentity(id: String) {
  require(id != null && id.length == 44 && id.endsWith("="))

  def publicKey: PublicKey = Ed25519Util.base64PublicKeyBytesToPublicKey(id)

  def toUid: Uid = Uid(id)
}

case class PrivateKeyPem(pemString: String) {
  def getBytes: Array[Byte] = pemString.getBytes(StandardCharsets.UTF_8)
}

case class CertificatePem(pemString: String) {
  def getBytes: Array[Byte] = pemString.getBytes(StandardCharsets.UTF_8)
}
