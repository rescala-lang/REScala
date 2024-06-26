package lofi_acl.crypto

import rdts.base.Uid

import java.security.PublicKey
import java.util.Base64

case class PublicIdentity(id: String) {
  require(id != null && id.length == 44 && id.endsWith("="))

  def publicKey: PublicKey = Ed25519Util.base64PublicKeyBytesToPublicKey(id)

  def toUid: Uid = Uid(id)
}

object PublicIdentity {
  private val base64Encoder = Base64.getEncoder

  def fromRawPublicKeyBytes(pubKeyBytes: Array[Byte]): PublicIdentity =
    require(pubKeyBytes.length == 32)
    PublicIdentity(base64Encoder.encodeToString(pubKeyBytes))

  def fromUid(uid: Uid): PublicIdentity = PublicIdentity(uid.delegate)

  extension (uid: Uid) inline def toPublicIdentity: PublicIdentity = PublicIdentity.fromUid(uid)
}
