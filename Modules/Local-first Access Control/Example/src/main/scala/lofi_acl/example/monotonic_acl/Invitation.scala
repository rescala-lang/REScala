package lofi_acl.example.monotonic_acl

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import lofi_acl.crypto.{Ed25519Util, PublicIdentity}

import java.security.{KeyPair, PrivateKey}
import java.util.Base64

case class Invitation(
    rootOfTrust: PublicIdentity,
    identityKey: KeyPair,
    inviter: PublicIdentity,
    joinAddress: String
)

object Invitation {
  private val base64Decoder = Base64.getDecoder
  private val base64Encoder = Base64.getEncoder

  def createInvite(
      rootOfTrust: PublicIdentity,
      inviter: PublicIdentity,
      joinAddress: String
  ): (PublicIdentity, Invitation) = {
    val createdPrincipalId = Ed25519Util.generateNewKeyPair
    val publicIdentity =
      PublicIdentity(Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(createdPrincipalId.getPublic))
    (publicIdentity, Invitation(rootOfTrust, createdPrincipalId, inviter, joinAddress))
  }

  def encode(invite: Invitation): String =
    val privateKeyBytes = Ed25519Util.privateKeyToRawPrivateKeyBytes(invite.identityKey.getPrivate)
    s"${invite.rootOfTrust}|${base64Encoder.encodeToString(privateKeyBytes)}|${invite.inviter.id}|${invite.joinAddress}"

  def decode(inviteString: String): Invitation = {
    val parts = inviteString.split('|')
    require(parts.length == 4)
    Invitation(
      PublicIdentity(parts(0)),
      Ed25519Util.rawPrivateKeyBytesToKeyPair(base64Decoder.decode(parts(1))),
      PublicIdentity(parts(2)),
      parts(3)
    )
  }
}
