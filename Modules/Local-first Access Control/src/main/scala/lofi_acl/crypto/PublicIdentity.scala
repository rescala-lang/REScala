package lofi_acl.crypto

import rdts.base.Uid

import java.security.PublicKey

case class PublicIdentity(id: String) {
  require(id != null && id.length == 44 && id.endsWith("="))

  def publicKey: PublicKey = Ed25519Util.base64PublicKeyBytesToPublicKey(id)

  def toUid: Uid = Uid(id)
}
