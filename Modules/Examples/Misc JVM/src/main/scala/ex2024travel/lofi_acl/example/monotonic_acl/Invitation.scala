package ex2024travel.lofi_acl.example.monotonic_acl

import lofi_acl.crypto.{Ed25519Util, PublicIdentity}
import ex2024travel.lofi_acl.example.monotonic_acl.Invitation.base64Encoder

import java.security.KeyPair
import java.util.Base64

case class Invitation(
    rootOfTrust: PublicIdentity,
    identityKey: KeyPair,
    inviter: PublicIdentity,
    joinAddress: String
) {
  def encode: String =
    val privateKeyBytes = Ed25519Util.privateKeyToRawPrivateKeyBytes(identityKey.getPrivate)
    s"${rootOfTrust.id}|${base64Encoder.encodeToString(privateKeyBytes)}|${inviter.id}|$joinAddress"
}

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
